% frag.erl -- Horizontal Database Fragments, that is a selection of rows
%
% The processes that are instantiated from this module represent a part of the
% overall database, selected as a subset of domain names.  The general idea is
% one of a hash, which may or may not match.
%
% The selection of the right dbfrag process is the concern of the routab, so
% all these processes do is rejecting mismatching answers, indeed with the
% {error,notfound} answer that is also used when no information is found on
% a given domain.  In both cases, it should be considered a sign that nothing
% can be done for the domain and further searching is fruitless.
%
% This is not a server behaviour; it is a functional wrapper that determines
% what database to use, and then forwards the call.  Note that remote access
% will have been caught out by the routab module before we get here, so all
% calls made are local ones.  The creation of the actual handling process
% for an authorisation request is done by the functions in the authz module.
%
% From: Rick van Rein <rick@openfortress.nl>


-module( frag ).

-export([
	init/1,
	loop/1
]).


-include( "donai.hrl" ).


% List the keys needed in the DbMap
%
% This even helps us across versions upgrades, because missing tables
% invalidate the heir that supplies them.  So, we get to rebuild the
% tables after such an upgrade -- which is usually the thing to do.
%
dbmap_keys() -> [
		pseudonymTab,
		aliasTab,
		flockTab,
		resourceTag,
		communicationTab
].


% Initialise the fragment management process for a given hash value
%
% A fragment is managed by first loading a current state for the
%
init( HashValue ) ->
		Pid_routab = whereis( routab ),
		State = #{
			hashval => HashValue,
			routab => Pid_routab,
			timeout => 0 },
		register_routab( State ),
		DbMap = init_dbmap( HashValue ),
		try getheir_frag( HashValue,DbMap )
		catch _ -> ok
		end,
		%TODO% or return State and hand back control to gen_server?
		loop( State#{dbmap=>DbMap} ).


% Loop around, attempting to register with the global "routab"
% process.  Fail until this is finally possible.  When it is,
% start monitoring it so we know that we shall have to return
% here when the process crashes and is to be replaced with a
% new incarnation.
%
register_routab( State=#{hashval:=HashValue,timeout:=Timeout} ) ->
		case whereis( routab ) of
		undefined ->
			receive after min( 1000,Timeout ) ->
				register_routab( State#{timeout:=2*Timeout} )
			end;
		Pid_routab ->
			_Ref = erlang:monitor( process,Pid_routab ),
			Pid_routab ! {add_local_fragment,[HashValue],self()},
			ok
		end.


% Initialise the database map.  An number of stragegies is tried:
%  1. Look for a heir and retrieve their heirloom
%  2. Look for network peers and retrieve data from them
%  3. Ask the SteamWorks Pulley configuration agent to reset our
%
% The strategies are defined below as init_dbmap_XXX functions.
% Each gets and returns a tuple of DbMap and a list of needed HashValues.
init_dbmap( HashValue ) ->
		{State,[]} = lists:foldl(
				fun( F,X ) ->
					try F( X )
					catch _ -> X
					end
				end,
				{undefined,[HashValue]},
				[ fun init_dbmap_heir/1,
				  fun init_dbmap_peers/1,
				  fun init_dbmap_steamworks/1 ] ),
		State.


% Initialise the database map from a heir.
init_dbmap_heir( Input={undefined,[HashValue]} ) ->
		{HashValue,{_,_,Pid_heir}} = ets:lookup( routing_table,HashValue ),
		Ref_heir = erlang:monitor( process,Pid_heir ),
		CollectValues = fun( CollectValues,KeyList ) ->
			case KeyList of
			[] ->
				[];
			[Key|KeyMore] ->
				% We specifically ask for each Key, to maximise the
				% chance of a crash and corresponding failure; better
				% here than further down the line where we are making
				% assumptions based on what would be false data
				Pid_heir ! {hand_over,Key},
				receive
					{'DOWN',Ref_heir,process,Pid_heir,_Reason} ->
						% Heir crashed; return too few Values
						[];
					{'ETS-TRANSFER',PassedTab,Pid_heir,Key} ->
						[PassedTab|CollectValues( KeyMore )]
				end
			end
		end,
		KeyList = dbmap_keys (),
		ValueList = CollectValues( CollectValues,KeyList ),
		Ref_heir = erlang:demonitor( Pid_heir,[flush] ),
		if ( length( ValueList ) < length( KeyList ) ) ->
			Input;
		true ->
			DbMap = maps:from_list( lists:zip( KeyList,ValueList ) ),
			DbMap
		end;
init_dbmap_heir( Other ) ->
		Other.

% Initialise the database map from peers.
%TODO% NOT IMPLEMENTED
init_dbmap_peers( Input ) ->
		Input.

% Initialise the database map from SteamWorks.
%TODO% NOT IMPLEMENTED
init_dbmap_steamworks( Input ) ->
		Input.


getheir_frag( HashValue,DbMap ) ->
		case ets:lookup( routing_table,HashValue ) of
		[] ->
			Opt = fun(_) -> {heir,none} end;
		[{HashValue,{_,_,none}}] ->
			Opt = fun(_) -> {heir,none} end;
		[{HashValue,{_,_,Pid_heir}}] ->
			Opt = fun(DbName) -> {heir,Pid_heir,DbName} end
		end,
		[ ets:set_opts( Db,Opt( DbName ) )
			|| {DbName,Db} <- maps:to_list( DbMap ) ],
		ok.


loop( State=#{hashval:=HashValue,routab:=Pid_routab,dbmap:=DbMap,timeout:=Timeout} ) ->
		%
		% main handler loop
		%
		receive
		%
		% The routab process goes down; register once more
		%
		{'DOWN',_Ref,process,Pid_routab,_Reason} ->
			register_routab( State#{routab:=undefined,Timeout:=5} );
		%
		% Some process sent us a ping.  Be nice and respond.
		{ping,Pid} ->
			Pid ! {pong,self()},
			loop( State );
		%
		% A process, presumably locally, wants to get our DbMap.
		% We do not facilitate the routab
		{dbget,Pid} ->
			Pid ! {dbgot,DbMap};
		%
		% When a new heir and frag come together, routab notifies us
		getheir_frag ->
			getheir_frag( HashValue,DbMap ),
			loop( State )
		%
		end.


%OLD% % Find a database fragment for a given hash outcome.
%OLD% %  - HashAlgorithms are tuples, like  {crc32mod,5}
%OLD% %  - HashValues     are tuples, like {{crc32mod,5},2}
%OLD% %  - The outcome is an index in a table that lists databases
%OLD% %  - The looked-up value is a map databases with databases under keys
%OLD% %     - pseudonymTab
%OLD% %     - aliasTab
%OLD% %     - flockTab
%OLD% %     - resourceTab
%OLD% %     - communicationTab
%OLD% %
%OLD% -spec find_dbfrag( hashalgo(),dom() ) -> #{}.
%OLD% %
%OLD% find_dbfrag( HashAlgo,Dom ) ->
%OLD% 		HashValue = compute_hash( HashAlgo,Dom ),
%OLD% 		{ HashValue,DbMap } = ets:lookup( allLocalDbFrag,HashValue ),
%OLD% 		DbMap.

