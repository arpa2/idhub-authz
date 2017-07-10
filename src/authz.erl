% authz.erl -- Authorisation main functions
%
% Authorisation under the InternetWide Architecture generally means:
%  1. Have an authenticated identity
%  2. Determine what authorisation identities are usable for it
%  3. Determine what ACLs apply to a given resource
%  4. Find what the authorisation identity that works with an ACL
%  5. Report access level based on this finding
%
% Variations exist, see ../doc/*.md and http://idp.arpa2.net/authz.html
%
% This module also estimates the cost of authorisation (there are more and
% less expensive operations), logs interesting things and possibly slows
% down the rate at which authorisation requests are fired at our systems.
%
% In style with Erlang design principles, we spawn a process for each
% individual request that we handle.  This keeps our code simple, and
% Erlang makes sure it nonetheless scales and distributes effortlessly.
% The process will be run on a node assigned to the target domain, as
% indicated in a routing table on the client-connected machine.
%
% From: Rick van Rein <rick@openfortress.nl>


-module( authz ).

-export([
	impose_as/4,
	resource_access/4,
	communication_access/4,
	spawn_worker/6,
	unspawn_worker/1,
	run_worker/6
]).

-include( "donai.hrl" ).
-include( "idmap.hrl" ).
-include( "xsmap.hrl" ).


%%
%% UTILITY FUNCTIONS
%%


% Cost is an estimate in an integer value
%
-type cost() :: integer().


% Estimate a cost for the current request.  We can use this for:
%  - rate control of bursty or agressive authorisation targets
%  - logging and alarms towards operational staff
%  - a measure of accounting access to the IdentityHub
%  - hints to forensic analysis, problem tracing, debugging
%
% Cost is estimated with:
%  - the length of the addresslist() from idmap.erl estimates its work
%  - the length of the accessibility() from xsmap.erl estimates its work
%  - with authzid, their product estimates work done here
%  - without authzid, the accessibility() length estimates work done here
% These numbers are simply added to come to a total cost estimate.
%
-spec cost( addresslist(),accessibility(),true|false ) -> cost().
%
cost( Ids,Xs,ViaAuthzId ) ->
		XsLengthAccu = fun( XsLengthAccu,XsMap,Accu ) ->
			case XsMap of
			[]               ->
				Accu;
			[{_,_,ACL}|More] ->
				XsLengthAccu(
					XsLengthAccu,
					More,
					Accu+length( ACL ) )
			end
		end,
		XsMapSize = XsLengthAccu( XsLengthAccu,Xs,0 ),
		IdMapSize = length( Ids ),
		case ViaAuthzId of
		true  -> XsMapSize + IdMapSize + IdMapSize * 1;
		false -> XsMapSize + IdMapSize + IdMapSize * XsMapSize
		end.


% Perform pre- and postprocessing for a given target domain.  This may be
% the place to slow down an incoming request, and learn from it afterwards.
%
-spec target_pre( db(),dom() ) -> none().
-spec target_post( db(),dom(),cost() ) -> none().
%
target_pre( _Db,_CurDom ) ->
		% ets:insert( maps:get( pseudonymTab,Db ), {Key,Value} )
		{}.
%
target_post( _Db,_CurDom,_Cost ) ->
		% case ets:lookup( maps:get( pseudonymTab,Db ), Key ) of [{Key,Value}] -> ...; [] -> ... end
		{}.


%INLINED% % Given a {Ref,Pid} pair, send the Ref to the Pid so it can be stored at
%INLINED% % that recipient.  This will allow the Pid to respond with Ref in the message,
%INLINED% % thereby simplifying the client code which can now always look for Ref.
%INLINED% %
%INLINED% % This function returns the Ref.  Having stored the Ref in the Pid, it can
%INLINED% % die without much effort.
%INLINED% %
%INLINED% % The receiving process should receive {ref2pid,Ref} to obtain the Ref.
%INLINED% % This can be done anytime desired, for instance just before responding.
%INLINED% %
%INLINED% -spec ref2pid( pid(),reference() ) -> reference().
%INLINED% %
%INLINED% ref2pid( Pid,Ref ) ->
%INLINED% 		% Deliberate lossy sending... monitoring was already setup
%INLINED% 		Pid ! {ref2pid,Ref},
%INLINED% 		Ref.
%INLINED% 
%INLINED% 
%INLINED% % A process that has been the target of ref2pid/2 can call this to learn
%INLINED% % the Ref sent to it.  This process must not be called more than once.
%INLINED% %
%INLINED% self_ref() ->
%INLINED% 		receive {ref2pid,Ref} ->
%INLINED% 			Ref.


%%
%% AUTHORISATION FUNCTIONS
%%


% Authorise a mapping from authentication identity to authorisation identity.
% This is a complete authorisation function.  The return value indicates the
% cost as well as the attained access level.
%
%TODO% May have to deal with string-form identities instead of adr() form.
%
-spec impose_as( db(),dom(),adr(),adr() ) -> {level_map(),adr()}.
%
impose_as( Db,CurDom,AuthnId,AuthzReqId ) ->
		target_pre( Db,CurDom ),
		IdMap = idmap:impose( Db,CurDom,AuthnId ),
		XsMap = [],
		Cost = cost( IdMap,XsMap,false ),
		Result = case lists:member( AuthzReqId,IdMap ) of
		true  -> {impose,  AuthzReqId};
		false -> {imposter,AuthnId}
		end,
		target_post( Db,CurDom,Cost ),
		Result.

% Authorise a mapping from authentication or prior-established authorisation
% identity to resource, going through an authorisation identity.
%
%TODO% May have to deal with string-form identities instead of adr() form.
%
-spec resource_access( db(),dom(),adr(),uuid() ) -> {level_res(),adr()}.
%
resource_access( Db,CurDom,AuthId,ResUUID ) ->
		target_pre( Db,CurDom ),
		IdMap = idmap:access( Db,CurDom,AuthId ),
		XsMap = xsmap:resource( Db,CurDom,ResUUID ),
		%TODO% Set "black" as the default level for no matches at all
		Result = xsmap:accessible( CurDom,IdMap,XsMap,black ),
		Cost = cost( IdMap,XsMap,false ),
		target_post( Db,CurDom,Cost ),
		Result.

% Authorise a mapping from authentication or prior-established authorisation
% identity to communication, going through an authorisation identity.
%
%TODO% May have to deal with string-form identities instead of adr() form.
%
%TODO% May strip away CurDom, unless TargetId can have domain set to current
%
-spec communication_access( db(),dom(),adr(),adr() ) -> {level_com(),adr()}.
%
communication_access( Db,CurDom,AuthId,TargetId ) ->
		target_pre( Db,CurDom ),
		IdMap = idmap:access( Db,CurDom,AuthId ),
		XsMap = xsmap:communication( Db,CurDom,TargetId ),
		%TODO% Set "gray" as the default level for no matches at all
		Result = xsmap:accessible( CurDom,IdMap,XsMap,gray ),
		Cost = cost( IdMap,XsMap,true ),
		target_post( Db,CurDom,Cost ),
		Result.

%%
%% PER-REQUEST AUTHORISATION PROCESSING
%%


% To define authorisation command tuples below, we need general handles
% for a function-tagging atom and an authorisation target.
%
-type authz_funtag() :: impose_as | resource_access | communication_access.
-type authz_target() :: adr() | uuid().


% The internal function run_worker/6 manages the process and sends a
% reply with a Ref instead of its Pid or another identity.  The Ref
% is assumed to have been collected during spawn_monitor/3 and is
% sent to the worker process for use when it wants to send a reply.
%
-spec run_worker( pid(),authz_funtag(),pid(),dom(),adr(),authz_target() ) -> none().
%
run_worker( ClientPid,AuthzFun,DbPid,CurDom,AuthnId,Target ) ->
		DbRef = erlang:monitor( process,DbPid ),
		DbPid ! {dbget,CurDom},
		receive
		{dbgot,Db} ->
			ok;
		{'DOWN',DbRef,process,DbPid,Reason} ->
			Db = erlang:error( Reason )
		end,
		erlang:demonitor( DbRef ),
		{Level,AuthzId} = case AuthzFun of
		impose_as ->
			impose_as(            Db,CurDom,AuthnId,Target);
		resource_access ->
			resource_access(      Db,CurDom,AuthnId,Target);
		communication_access ->
			communication_access( Db,CurDom,AuthnId,Target)
		end,
		receive {ref2pid,Ref} ->
			Response = {authz,Ref,Level,AuthzId},
			ClientPid ! Response	% Pleasantly lossy
		end,
		{}.


% Spawn a worker process for an authorisation task.  The function returns
% a Ref for a monitor that guards the authorisation process.  The caller
% should store it in a key-value store to retrieve upon completion.
%
%TODO% Oversight, we always get a Pid, also with 'DOWN', so ignore Ref
%	_except_ that erlang:demonitor needs the Ref
%TODO% Client is an agent, running on behalf of just one query, so OK
%	to return Pid as well as Ref; no need for DB-mapping
%
% When done, the authorisation worker sends the caller one of these:
%
%  1. {'DOWN',Ref,process,Pid,Reason}
%
%       The Ref may be removed from the key-value store;
%	Reason may be noproc or killed on error.
%
%  2. {authz,Ref,level(),adr()}
%
%	The Ref may be removed from the key-value store;
%	A 'DOWN' message may still follow (probably with Reason 'normal');
%	The level() and adr() are the Result from one of the routines
%	impose_as/4, resource_access/4, communication_access/4.
%
% At least after receiving the {authz,...} response (and if you like after
% the {'DOWN',...} response) please invoke unspawn_worker/1 with the Ref
% that this function delivers.  This guarantees that a possible extra
% {'DOWN',...} message is avoided or suppressed, whichever it takes.
%
%TODO% Use spawn_opt(Node,?MODULE,run_worker,Args,[monitor]) to reach remotes
%
-spec spawn_worker( node(),authz_funtag(),pid(),dom(),adr(),authz_target() ) -> reference().
%
spawn_worker( Node,AuthzFun,DbPid,CurDom,AuthnId,Target ) ->
		{Pid,Ref} = erlang:spawn_opt(
			Node,
			?MODULE, run_worker,
			{ self(),AuthzFun,DbPid,CurDom,AuthnId,Target },
			[monitor] ),
		Pid ! {ref2pid,Ref},	% Pleasantly lossy
		Ref.


% unspawn_worker/1 reverses the work of spawn_worker, by blocking the
% reception of further monitor messages from the spawned process.
% The worker independently continues to run to its natural termination.
%
-spec unspawn_worker( reference() ) -> none().
%
unspawn_worker( Ref ) ->
		erlang:demonitor( Ref,[flush] ).


