% heir.erl -- Database heirs hold on to them while parent reincarnate
%
% The ETS tables used in IdHub Authz disappear from memory when their owner
% process dies.  To avoid reloading them from scratch on glitches, with the
% related performance degredation and thus DoS opportunity, we assign a heir
% process that has nothing more to do but to collect {'ETS-TRANSFER'...}
% indications, only to send them to a newly announced owner for that table.
%
% The HeirData is the Key used in the DbMap, if any.  The heir responds to
% requests formed like {hand_over,Key} by giving them away to the requester,
% where they arrive as {'ETS-TRANSFER',...} messages.  The heir will not
% terminate when it has sent back everything, but simply continue as a heir.
% It may however crash when it cannot find a Key, possibly because it holds
% a wrong dataset, or because the table with that Key has already been
% handed out to another process that did not hand it back to us.  It is
% because of this presumed giving-back that we shall remove any Key that we
% have delivered to another process.
%
% From: Rick van Rein <rick@openfortress.nl>


-module( heir ).

%IDEA%
%IDEA% Just an idea: the heir could start off with tables before frag/routab
%IDEA% After all, routab knows when coverage == 100% with local/remote frag
%IDEA% After all, frag   knows when its full table has been recovered or loaded
%IDEA%

-export([
	init/1,
	loop/1
]).


init( HashValue ) ->
		Pid_routab = register_routab( HashValue,1 ),
		DbMap = #{},
		%TODO% Or return state to the gen_server
		loop( {HashValue,DbMap,Pid_routab} ).


% Loop around, attempting to register with the global "routab"
% process.  Fail until this is finally possible.  When it is,
% start monitoring it so we know that we shall have to return
% here when the process crashes and is to be replaced with a
% new incarnation.
%
register_routab( HashValue,Timeout ) ->
		case whereis( routab ) of
		undefined ->
			receive after min( 1000,Timeout ) ->
				register_routab( HashValue,2*Timeout )
			end;
		Pid_routab ->
			_Ref = erlang:monitor( process,Pid_routab ),
			case HashValue of
			routab ->
				Pid_routab ! {routab_heir,self()};
			_Tuple ->
				Pid_routab ! {add_local_fragment_heir,HashValue,self()}
			end,
			Pid_routab
		end.


loop( {HashValue,DbMap,Pid_routab} ) ->
		receive
		%
		% The routab process goes down; register once more
		%
		{'DOWN',_Ref,process,Pid_routab,_Reason} ->
			NewPid_routab = register_routab( HashValue,1 ),
			NewDbMap = DbMap;
		%
		% We are sent a table, presumably because our parent dies
		%
		{'ETS-TRANSFER',Table,_FromPid,DbMapKey} ->
			% We will crash on repeated submissions of a table
			% to avoid curious synchronisation and ordering errors
			NewDbMap = DbMap#{DbMapKey=>Table},
			NewPid_routab = Pid_routab;
		%
		% Some process, presumably our parent, requests table hand_over
		%
		{hand_over,DbMapKey} ->
			% We will crash on requests for unknown keys
			% to avoid clumsy work-arounds for failing data
			% or erroneous communication structures; we also
			% require registration for the new parent
			Pid_self = self(),
			{Pid_frag,_,Pid_self} = ets:lookup( routing_table,HashValue ),
			{Table,NewDbMap} = maps:take( DbMapKey,DbMap ),
			ets:give_away( Table,Pid_frag,DbMapKey ),
			NewPid_routab = Pid_routab
		end,
		loop( {HashValue,NewDbMap,NewPid_routab} ).

