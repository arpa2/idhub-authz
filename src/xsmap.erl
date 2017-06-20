%% Access Controller for ARPA2 IdentityHub Authorisation
%%
%% This process answers "May this user use this Resource?"
%%
%% From: Rick van Rein <rick@openfortress.nl>


-module(xsmap).

-export([
	init/0,
	service_init/0,
	authz/2
	]).


-include("donai.hrl").


%% The "xsmap" database holds compiled Access Control Lists to resources.
%%
%% Resources may also be a marker for a particular service, such as a
%% web server.  In this case, a separate list is available for various
%% levels of access, and the most empowering level will be reported.
%% (The access levels have a complete order in terms of empowering.)
%% See doc/authz-internal-design-spec.md for details on the levels.
%%
%% Resources model the ability to communicate, using any protocol, with
%% the users of a domain.  This is established with black, white and
%% gray lists, and the communication request is either reported as
%% one of these levels.
%%
%% In all cases, it is assumed that the requester will interpret the
%% reported grant level, and probably retain the data for future use
%% in the same session with an authenticated user.  One might say that
%% every portal to internal functions, every service as viewed from
%% the "outside" network, is a Network Access Server.
%%
%% The terms in the "xsmap" model these lists in a compiled form, to
%% aid efficient handling.  The first list to provide a match determines
%% the outcome of the operation; this is possible because all available
%% outcomes can be setup explicitly with their own Access Control List.


%% The levels that can be assigned to communication / resources
%%
-type level_com() :: white | gray | black.
-type level_res() :: admin | service |
                     delete | create | write | read |
                     prove | know | own |
                     visit.
-type level()     :: level_com() | level_res().

%% Resources will be identified through a UUID in binary form
%%
-type uuid() :: binary().

%% Client-supplied line numbers for ACLs help to insert, edit and delete ACLs
%%
-type lineno() :: integer().

%% Match operations output a (possibly empty) list of matches
%%
-type matches() :: adr() | nomatch.

%% Access Control Lists hold DoNAI Selectors for a given level
%%
-type acl() :: [ adrsel() ].

%% The keys for looking up ACLs; to be stored in the process dictionary
%% Both kinds of keys include the domain, in support of multi-tenant service
%%
-type pd_key() :: { communication, fqn() } |
                  { resource, dom(), uuid() }.

%% The values in the process dictionary detail the various ACLs
%%  - pk_key :: { communication, _ } implies pd_val :: [ { level_com(), _ } ]
%%  - pk_key :: { resource,      _ } implies pd_val :: [ { level_res(), _ } ]
%%  - levels() in pd_val appear in an order of descending empowerment
%%
-type pd_val() :: [ { lineno(), level(), acl() } ].


%%
%% GENERIC AUTHORISATION FUNCTIONS
%%


%% Walk through a pd_val(), testing for identity matches, and
%% deliver the level for the first that matches.
%%
%% There are rules for one/none/both lists in the documentation
%% in doc/authz-2-kinds.md -- these should be implemented when the
%% pd_val() is constructed.
%%
-spec pdval2level( level(), adr(), pd_val() ) -> level().
%%
pdval2level( Level0,_Adr,[] ) ->
		Level0;
pdval2level( Level0,Adr,[{_,Level,ACL}|More] ) ->
		case match_acl( Adr,ACL ) of
		nomatch ->
			pdval2level( Level0,Adr,More );
		_ ->
			Level
		end.

%% match_acl/2 returns an address match from an ACL.
%%
-spec match_acl( adr(), acl() ) -> matches().
%%
match_acl( _Adr,[] ) -> nomatch;
match_acl( Adr,[AdrSel|ACL] ) ->
		io:write( Adr ),
		io:write( <<"   ">> ),
		io:write( AdrSel ),
		io:nl(),
		case donai:compare_adrsel2adr (Adr,AdrSel) of
		true ->
			io:write( <<"MATCHED">> ),
			io:nl(),
			Adr;
		false ->
			io:write( <<"...">> ),
			io:nl(),
			match_acl( Adr,ACL )
		end.

%%
%% PROTOCOL OPERATION
%%

%% Initialise the Process Dictionary by erasing it, then start the service loop
%%
-spec init() -> none().
%%
init() ->
	erase(),
	%TODO% Insert test data -- FIXED
	NewPid = spawn( ?MODULE, service_init, [] ),
	_RegRes = register( arpa2_authz_xsmap,
		NewPid).

service_init() ->
	put( { communication, <<"john@example.com">> },
		[ { 10, white, [ donai:parse_fqnsel2adrsel (<<"@.com">>) ] },
		  { 20, gray,  [ donai:parse_fqnsel2adrsel (<<"mary@.">>) ] } ] ),
	io:write( get() ),
	io:nl(),
	service_loop().


%% The service loop accepts the following messages:
%%  - acl_add/del_entry to add/delete entries from an ACL
%%  - authz_xsmap to attempt authorisation
-spec service_loop() -> none().
%%
service_loop() ->
	receive
	{ acl_add_entry, Pid, PDKey, LineNo, Level, FQNSel } ->
		PDVal = get( PDKey ),
		NewPDVal = service_acl_insert_entry (PDVal, LineNo, Level, FQNSel),
		Return = ok,
		put( PDKey, NewPDVal ),
		Pid ! Return;
	{ acl_del_entry, Pid, PDKey, LineNo, Level, FQNSel } ->
		PDVal = get( PDKey ),
		NewPDVal = service_acl_remove_entry (PDVal, LineNo, Level, FQNSel),
		Return = ok,
		put( PDKey, NewPDVal ),
		Pid ! Return;
	{ authz_xsmap, Pid, PDKey, FQN } ->
		PDVal = get( PDKey ),
		Return = service_authz_xsmap (PDVal, FQN),
		Pid ! Return
	end,
	service_loop().


%% Add an entry to an ACL; insert the ACL if it does not exist
%%
%%TODO%% More elaborate return values :-)
%%TODO%% Readability >:-(
%%
-spec service_acl_insert_entry( pd_val(), lineno(), level(), fqnsel() ) -> pd_val().
%%
service_acl_insert_entry( [{ValLineNo,ValLevel,ValSel}|MoreVal], NewLineNo, NewLevel, NewFQNSel )
		when ValLineNo < NewLineNo ->
		[{ValLineNo,ValLevel,ValSel}|service_acl_insert_entry (MoreVal, NewLineNo, NewLevel, NewFQNSel)];
service_acl_insert_entry( [{NewLineNo,NewLevel,ValSel}|MoreVal], NewLineNo, NewLevel, NewFQNSel ) ->
		[{NewLineNo,NewLevel,[NewFQNSel|ValSel]}|MoreVal];
service_acl_insert_entry( RestVal, NewLineNo, NewLevel, NewFQNSel ) ->
		service_acl_insert_entry( [{NewLineNo,NewLevel,[]}|RestVal], NewLineNo, NewLevel, NewFQNSel ).


%% Remote an entry from an ACL; remove the ACL if it ends up being empty
%%
%%TODO%% More elaborate return values :-)
%%TODO%% Readability >:-(
%%
-spec service_acl_remove_entry( pd_val(), lineno(), level(), fqnsel() ) -> pd_val().
%%
service_acl_remove_entry( [{NewLineNo,NewLevel,[NewFQNSel|ValSet]}|MoreVal], NewLineNo, NewLevel, NewFQNSel ) ->
		service_acl_remove_entry( [{NewLineNo,NewLevel,ValSet}|MoreVal], NewLineNo, NewLevel, NewFQNSel );
service_acl_remove_entry( [{NewLineNo,NewLevel,[]}|MoreVal], NewLineNo, NewLevel, _ ) ->
		MoreVal;
service_acl_remove_entry( [{ValLineNo,ValLevel,ValSel}|MoreVal], NewLineNo, NewLevel, NewFQNSel )
		when ValLineNo < NewLineNo ->
		[{ValLineNo,ValLevel,ValSel}|service_acl_remove_entry (MoreVal, NewLineNo, NewLevel, NewFQNSel)];
service_acl_remove_entry( [], _, _, _) ->
		[].


%% Attempt authorisation based on the ACL logic
%%
%%TODO%% More elaborate return values :-)
%%
-spec service_authz_xsmap( pd_val(), fqn() ) -> { ok, level() }.
%%
service_authz_xsmap( PDVal, FQN ) ->
		Adr = donai:parse_fqn2adr( FQN ),
		io:write( <<"PDVal">> ),
		io:write( PDVal ),
		io:nl(),
		% pdval2level( undefined, Adr, PDVal ).
		pdval2level( confused, Adr, PDVal ).


%%
%% LIBRARY ROUTINES TO ACCESS THE PROCESS
%%


%% Send an xsmap authorisation request to the process.
%%
-spec authz( pd_key(), fqn() ) -> ok.
%%
authz( PDKey, FQN ) ->
		arpa2_authz_xsmap ! { authz_xsmap, self(), PDKey, FQN },
		receive
		ok ->
			io:write( "Hoera!" ),
			io:nl();
		Reply ->
			io:write( "..." ),
			io:write( Reply ),
			io:nl()
		end.


