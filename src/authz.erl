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
	communication_access/4
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
cost( Adrs,Xs,ViaAuthzId ) ->
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
		IdMapSize = length( Adrs ),
		case ViaAuthzId of
		true  -> XsMapSize + IdMapSize + IdMapSize * 1;
		false -> XsMapSize + IdMapSize + IdMapSize * XsMapSize
		end.


% Perform pre- and postprocessing for a given target domain.  This may be
% the place to slow down an incoming request, and learn from it afterwards.
%
-spec target_pre( dom() ) -> none().
-spec target_post( dom(),cost() ) -> none().
%
target_pre( _CurDom ) ->
		{}.
%
target_post( _CurDom,_Cost ) ->
		{}.


% Authorise a mapping from authentication identity to authorisation identity.
% This is a complete authorisation function.  The return value indicates the
% cost as well as the attained access level.
%
%TODO% May have to deal with fqn() form instead of adr() form.
%
-spec impose_as( db(),dom(),adr(),adr() ) -> {level_map(),adr()}.
%
impose_as( Db,CurDom,AuthnId,AuthzReqId ) ->
		target_pre( CurDom ),
		IdMap = idmap:impose( Db,CurDom,AuthnId ),
		XsMap = [],
		Cost = cost( IdMap,XsMap,false ),
		Result = case lists:member( IdMap,AuthzReqId ) of
		true  -> {impose,  AuthzReqId};
		false -> {imposter,AuthnId}
		end,
		target_post( CurDom,Cost ),
		Result.

% Authorise a mapping from authentication or prior-established authorisation
% identity to resource, going through an authorisation identity.
%
-spec resource_access( db(),dom(),adr(),uuid() ) -> {level_res(),adr()}.
%
resource_access( Db,CurDom,AuthId,ResUUID ) ->
		target_pre( CurDom ),
		IdMap = idmap:access( Db,CurDom,AuthId ),
		XsMap = xsmap:resource( Db,CurDom,ResUUID ),
		Result = xsmap:accessible( IdMap,XsMap,visitor ),
		Cost = cost( IdMap,XsMap,false ),
		target_post( CurDom,Cost ),
		Result.

% Authorise a mapping from authentication or prior-established authorisation
% identity to communication, going through an authorisation identity.
%
-spec communication_access( db(),dom(),adr(),adr() ) -> {level_com(),adr()}.
%
communication_access( Db,CurDom,AuthId,TargetId ) ->
		target_pre( CurDom ),
		IdMap = idmap:access( Db,CurDom,AuthId ),
		XsMap = xsmap:communication( Db,CurDom,TargetId ),
		Result = xsmap:accessible( IdMap,XsMap,black ),
		Cost = cost( IdMap,XsMap,true ),
		target_post( CurDom,Cost ),
		Result.


