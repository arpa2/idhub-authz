% agent.erl -- Generic Agent for IdentityHub Authorisation
%
% This module runs a process dedicated to a user request.
% The agent takes in a parsed Diameter message, constructs an
% internal authorisation request and looks in the routab for
% targets to send to.  It will try each one it finds in turn,
% until one responds.  Specifically, it will monitor for the
% failure of a process while awaiting a response.  Upon a
% detected failure, the message will be forwarded to the next
% in line.  Finally, a default value will be responded.
%
% When handling the routab, the agant notices entries for
% machines that may be in trouble.  It may nonetheless try
% these entires, and report either {commfailure,...} or
% {commsuccess,...} to the responsible routab, where the
% errors may be counted and the entries removed from the
% routab table.
%
% TODO:FUTURE: In later releases, we may want to route more
% dynamically by taking prior response times into account.
%
% From: Rick van Rein <rick@openfortress.nl>


-module( agent ).



remote_authz( [],{AuthzFun,_,AuthnId,_} ) ->
		DefaultLevel = case AuthzFun of
		impose_as -> imposter;
		resource_access -> black;
		communication_access -> gray
		end,
		{DefaultLevel,AuthnId};
remote_authz( [undefined|DbMore],Args ) ->
		remote_authz( DbMore,Args );
remote_authz( [DbPid|DbMore],Args={AuthzFun,CurDom,AuthnId,Target} ) ->
		Node = erlang:node( DbPid ),
		HdlRef = authz:spawn_worker( 
			Node,AuthzFun,DbPid,
			CurDom,AuthnId,Target ),
		receive
		{'DOWN',HdlRef,process,_Pid,_Reason} ->
			%TODO% Log report (handlers normally succeed)
			remote_authz( DbMore,Args );
		{authz,HdlRef,Level,AuthzId} ->
			authz:unspawn_worker( HdlRef,[flush] ),
			{Level,AuthzId}
		end.


%OLD%first_worker( [],_Routab,_Args ) ->
%OLD%	%TODO% or: send a {giveup,Routab,Args} message to self()
%OLD%	%TODO% DIAMETER_UNABLE_TO_DELIVER 3002 -> {protocol_error,3002}
%OLD%	nomore,
%OLD%first_worker( [Node|_]=Nodes,Routab,Args ) ->
%OLD%	Ref = authz:spawn_worker( AuthzFun,Db,CurDom,AuthnId,Target )
%OLD%	% Leave recovery data in the process dictionary
%OLD%	put( Ref,{charged_worker,Nodes,Routab,Args ),
%OLD%	% Done, a process was spawned and should report back to us
%OLD%	Ref.
%OLD%
%OLD%next_worker( Ref ) ->
%OLD%	% Give up the monitor (TODO: necessary after a failure report?)
%OLD%	authz:unspawn_worker( Ref ),
%OLD%	% Take worker state out of the process dictionary
%OLD%	case get( Ref ) of
%OLD%	undefined ->
%OLD%		NextNodes = Routab = Args = [];
%OLD%	{charged_worker,[FailedNode|NextNodes],Routab,{_,_,CurDom,_,_}=Args} ->
%OLD%		% Clear this entry from the process dictionary
%OLD%		erase( Ref ),
%OLD%		% Inform the routab that inspired us, but gently tolerate failures
%OLD%		ets:info( Routab,owner ) ! {commfailure,CurDom,FailedNode},
%OLD%	end,
%OLD%	% Now start the first remaining worker -- or fail when none left
%OLD%	first_worker( NextNodes,Routab,Args ).
%OLD%
%OLD%% Respond to a worker going down with {'DOWN',Ref,process,Reason}
%OLD%%
%OLD%failing_worker( Ref,Reason ) ->
%OLD%	case next_worker( Ref ) of
%OLD%	NewRef ->
%OLD%		NewRef
%OLD%	nomore ->
%OLD%		%TODO% Formulate Diameter negative answer
%OLD%		nomore
%OLD%
%OLD%% Respond to a worker succeeding with {authz,Ref,Level,UseThisAdr}
%OLD%%
%OLD%success_worker( Ref,Level,UseThisAdr ) ->
%OLD%	% Give up the monitor (which may not have reported yet)
%OLD%	authz:unspawn_worker( Ref ),
%OLD%	% Take worker state out of the process dictionary
%OLD%	{charged_worker,[FailedNode|NextNodes],Routab,{_,_,CurDom,_,_}=Args} = get( Ref ),
%OLD%	% Clear this entry from the process dictionary
%OLD%	erase( Ref ),
%OLD%	% Inform the routab that inspired us, but gently tolerate failures
%OLD%	ets:info( Routab,owner ) ! {commfailure,CurDom,FailedNode},
%OLD%	% Construct Diameter response
%OLD%	diameter_TODO,
%OLD%	% Send Diameter response
%OLD%	diameter_TODO.
%OLD%
%OLD%
%OLD%% Initialise the handler loop
%OLD%%
%OLD%init( Args ) ->
%OLD%	% Clean up the process dictionary
%OLD%	erase(),
%OLD%	% Return initial state
%OLD%	#{}.
%OLD%
%OLD%% Receive the next message and handle it
%OLD%%
%OLD%loop( State ) ->
%OLD%	receive
%OLD%	%
%OLD%	% Messages from spawned workers
%OLD%	%
%OLD%	{'DOWN',Ref,process,Reason} ->
%OLD%		failing_worker( Ref,Reason );
%OLD%	{authz,Ref,Level,UseThisAdr} ->
%OLD%		success_worker( Ref,Level,UseThisAdr )
%OLD%	%TODO% perhaps a 'giveup' message from myself...
%OLD%	%
%OLD%	%TODO% diameter incoming messages...
%OLD%	%
%OLD%	end,
%OLD%	State.
%OLD%
%OLD%
