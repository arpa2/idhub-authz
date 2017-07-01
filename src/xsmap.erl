%% Access Controller for ARPA2 IdentityHub Authorisation
%%
%% This process answers "May this user use this Resource?"
%%
%% From: Rick van Rein <rick@openfortress.nl>


-module( xsmap ).

-export([
	%TODO% init/0
	%TODO% ,service_init/0
	%TODO% ,authz/2
	resource/4,
	communication/4,
	accessible/3
	]).


-include( "donai.hrl" ).
-include( "idmap.hrl" ).
-include( "xsmap.hrl" ).


%% The "xsmap" database holds compiled Access Control Lists to resources.
%%
%% Resources may also be a marker for a particular service, such as a
%% web server.  In this case, a separate list is available for various
%% levels of access, and usually the most empowering level is desired.
%% (The access levels have a complete order in terms of empowering.)
%% See doc/authz-internal-design-spec.md for details on the levels.
%%
%% In the implementation in this module, it is assumed that ACLs are
%% sorted in the order of declining access level to achieve the highest
%% possible levels; but this is not the only possibility.  In other
%% words, such expectancies are implemented in the software driving
%% ACLs into this xsmap module.
%%
%% Communication models the ability to chat, using any protocol, with
%% the users of a domain.  This is established with black, white and
%% gray lists, and the communication request is reported as one of
%% these levels.
%%
%% In all cases, it is assumed that the requester will interpret the
%% reported grant level, and probably retain the data for future use
%% in the same session with an authenticated user.  It remains up to
%% the application protocol to fence off a session.  One might say that
%% every portal to internal functions, every service as viewed from
%% the "outside" network, is a Network Access Server in terms of the
%% Diameter and RADIUS protocols.
%%
%% The terms of the "xsmap" model holds on to ACLs in a compiled form, to
%% aid efficient handling.  The first list to provide a match determines
%% the outcome of the operation; this is possible because all available
%% outcomes can be setup explicitly with their own Access Control List.


%OLD% %% Match operations output a matching address or the atom nomatch
%OLD% %%
%OLD% -type matches() :: adr() | nomatch.

%OLD% %% The keys for looking up ACLs; to be stored in the process dictionary
%OLD% %% Both kinds of keys include the domain, in support of multi-tenant service
%OLD% %%
%OLD% -type pd_key() :: { communication, fqn() } |
%OLD%                   { resource, dom(), uuid() }.
%OLD% 
%OLD% %% The values in the process dictionary detail the various ACLs
%OLD% %%  - pk_key :: { communication, _ } implies pd_val :: [ { level_com(), _ } ]
%OLD% %%  - pk_key :: { resource,      _ } implies pd_val :: [ { level_res(), _ } ]
%OLD% %%  - levels() in pd_val appear in an order of descending empowerment
%OLD% %%
%OLD% -type pd_val() :: [ { lineno(), level(), acl() } ].


%%
%% GENERIC AUTHORISATION FUNCTIONS
%%


%OLD% %% Walk through a pd_val(), testing for identity matches, and
%OLD% %% deliver the level for the first that matches.
%OLD% %%
%OLD% %% There are rules for one/none/both lists in the documentation
%OLD% %% in doc/authz-2-kinds.md -- these should be implemented when the
%OLD% %% pd_val() is constructed.
%OLD% %%
%OLD% -spec pdval2level( level(), adr(), pd_val() ) -> level().
%OLD% %%
%OLD% pdval2level( Level0,_Adr,[] ) ->
%OLD% 		Level0;
%OLD% pdval2level( Level0,Adr,[{_,Level,ACL}|More] ) ->
%OLD% 		case match_acl( Adr,ACL ) of
%OLD% 		nomatch ->
%OLD% 			pdval2level( Level0,Adr,More );
%OLD% 		_ ->
%OLD% 			Level
%OLD% 		end.

%OLD% %% match_acl/2 returns an address match from an ACL.
%OLD% %%
%OLD% -spec match_acl( adr(), acl() ) -> matches().
%OLD% %%
%OLD% match_acl( _Adr,[] ) -> nomatch;
%OLD% match_acl( Adr,[AdrSel|ACL] ) ->
%OLD% 		io:write( Adr ),
%OLD% 		io:write( <<"   ">> ),
%OLD% 		io:write( AdrSel ),
%OLD% 		io:nl(),
%OLD% 		case donai:compare_adrsel2adr (Adr,AdrSel) of
%OLD% 		true ->
%OLD% 			io:write( <<"MATCHED">> ),
%OLD% 			io:nl(),
%OLD% 			Adr;
%OLD% 		false ->
%OLD% 			io:write( <<"...">> ),
%OLD% 			io:nl(),
%OLD% 			match_acl( Adr,ACL )
%OLD% 		end.

%% accessible/3 returns an address match from an ACL
%% as the level of the match and the matching address.
%% Even when multiple matches are possible, the first
%% one will be returned.  Multiple adresses are
%% usually supplied to pass over the accessibility
%% information.  It is an error to supply no
%% addresses at all (no reply can then be formed.)
%%
-spec accessible( [adr()],accessibility(),level() ) -> { adr(),level() }.
%%
accessible( [Adr|_],[],DefaultLevel ) ->
		{Adr,DefaultLevel};
accessible( [_|_]=Adrs,[{_,Level,ACL}|NextLevel],DefaultLevel ) ->
		accessible( Adrs,Level,ACL,NextLevel,DefaultLevel ).
%%
-spec accessible( [adr()],level(),acl(),accessibility(),level() ) -> { adr(),level() }.
%%
accessible( Adrs,_CurrentLevel,[],NextLevel,DefaultLevel ) ->
		% back to the simple function form, for the outer list
		accessible( Adrs,NextLevel,DefaultLevel );
accessible( Adrs,CurrentLevel,[AdrSel|ACL],NextLevel,DefaultLevel ) ->
		NotSelected = fun( Adr ) ->
			not donai:compare_adrsel2adr( AdrSel,Adr )
		end,
		case lists:dropwhile( NotSelected,Adrs ) of
		[]      -> 
			io:format( "...~n" ),
			accessible( Adrs,CurrentLevel,ACL,NextLevel,DefaultLevel );
		[Adr|_] ->
			io:format( "MATCHED ~p~n",[Adr] ),
			{Adr,CurrentLevel}
		end.


%%
%% PROTOCOL OPERATION
%%

%OLD% %% Initialise the Process Dictionary by erasing it, then start the service loop
%OLD% %%
%OLD% -spec init() -> none().
%OLD% %%
%OLD% init() ->
%OLD% 	erase(),
%OLD% 	%TODO% Insert test data -- FIXED
%OLD% 	NewPid = spawn( ?MODULE, service_init, [] ),
%OLD% 	_RegRes = register( arpa2_authz_xsmap,
%OLD% 		NewPid).

%OLD% service_init() ->
%OLD% 	put( { communication, <<"john@example.com">> },
%OLD% 		[ { 10, white, [ donai:parse_fqnsel2adrsel (<<"@.com">>) ] },
%OLD% 		  { 20, gray,  [ donai:parse_fqnsel2adrsel (<<"mary@.">>) ] } ] ),
%OLD% 	io:write( get() ),
%OLD% 	io:nl(),
%OLD% 	service_loop().


%OLD% %% The service loop accepts the following messages:
%OLD% %%  - acl_add/del_entry to add/delete entries from an ACL
%OLD% %%  - authz_xsmap to attempt authorisation
%OLD% -spec service_loop() -> none().
%OLD% %%
%OLD% service_loop() ->
%OLD% 	receive
%OLD% 	{ acl_add_entry, Pid, PDKey, LineNo, Level, FQNSel } ->
%OLD% 		PDVal = get( PDKey ),
%OLD% 		NewPDVal = service_acl_insert_entry (PDVal, LineNo, Level, FQNSel),
%OLD% 		Return = ok,
%OLD% 		put( PDKey, NewPDVal ),
%OLD% 		Pid ! Return;
%OLD% 	{ acl_del_entry, Pid, PDKey, LineNo, Level, FQNSel } ->
%OLD% 		PDVal = get( PDKey ),
%OLD% 		NewPDVal = service_acl_remove_entry (PDVal, LineNo, Level, FQNSel),
%OLD% 		Return = ok,
%OLD% 		put( PDKey, NewPDVal ),
%OLD% 		Pid ! Return;
%OLD% 	{ authz_xsmap, Pid, PDKey, FQN } ->
%OLD% 		PDVal = get( PDKey ),
%OLD% 		Return = service_authz_xsmap (PDVal, FQN),
%OLD% 		Pid ! Return
%OLD% 	end,
%OLD% 	service_loop().


%OLD% %% Add an entry to an ACL; insert the ACL if it does not exist
%OLD% %%
%OLD% %%TODO%% More elaborate return values :-)
%OLD% %%TODO%% Readability >:-(
%OLD% %%
%OLD% -spec service_acl_insert_entry( pd_val(), lineno(), level(), fqnsel() ) -> pd_val().
%OLD% %%
%OLD% service_acl_insert_entry( [{ValLineNo,ValLevel,ValSel}|MoreVal], NewLineNo, NewLevel, NewFQNSel )
%OLD% 		when ValLineNo < NewLineNo ->
%OLD% 		[{ValLineNo,ValLevel,ValSel}|service_acl_insert_entry (MoreVal, NewLineNo, NewLevel, NewFQNSel)];
%OLD% service_acl_insert_entry( [{NewLineNo,NewLevel,ValSel}|MoreVal], NewLineNo, NewLevel, NewFQNSel ) ->
%OLD% 		[{NewLineNo,NewLevel,[NewFQNSel|ValSel]}|MoreVal];
%OLD% service_acl_insert_entry( RestVal, NewLineNo, NewLevel, NewFQNSel ) ->
%OLD% 		service_acl_insert_entry( [{NewLineNo,NewLevel,[]}|RestVal], NewLineNo, NewLevel, NewFQNSel ).


%OLD% %% Remove an entry from an ACL; remove the ACL if it ends up being empty
%OLD% %%
%OLD% %%TODO%% More elaborate return values :-)
%OLD% %%TODO%% Readability >:-(
%OLD% %%
%OLD% -spec service_acl_remove_entry( pd_val(), lineno(), level(), fqnsel() ) -> pd_val().
%OLD% %%
%OLD% service_acl_remove_entry( [{NewLineNo,NewLevel,[NewFQNSel|ValSet]}|MoreVal], NewLineNo, NewLevel, NewFQNSel ) ->
%OLD% 		service_acl_remove_entry( [{NewLineNo,NewLevel,ValSet}|MoreVal], NewLineNo, NewLevel, NewFQNSel );
%OLD% service_acl_remove_entry( [{NewLineNo,NewLevel,[]}|MoreVal], NewLineNo, NewLevel, _ ) ->
%OLD% 		MoreVal;
%OLD% service_acl_remove_entry( [{ValLineNo,ValLevel,ValSel}|MoreVal], NewLineNo, NewLevel, NewFQNSel )
%OLD% 		when ValLineNo < NewLineNo ->
%OLD% 		[{ValLineNo,ValLevel,ValSel}|service_acl_remove_entry (MoreVal, NewLineNo, NewLevel, NewFQNSel)];
%OLD% service_acl_remove_entry( [], _, _, _) ->
%OLD% 		[].


%OLD% %% Attempt authorisation based on the ACL logic
%OLD% %%
%OLD% %%TODO%% More elaborate return values :-)
%OLD% %%
%OLD% -spec service_authz_xsmap( pd_val(), fqn() ) -> { ok, level() }.
%OLD% %%
%OLD% service_authz_xsmap( PDVal, FQN ) ->
%OLD% 		Adr = donai:parse_fqn2adr( FQN ),
%OLD% 		io:write( <<"PDVal">> ),
%OLD% 		io:write( PDVal ),
%OLD% 		io:nl(),
%OLD% 		% pdval2level( undefined, Adr, PDVal ).
%OLD% 		pdval2level( confused, Adr, PDVal ).


%%
%% LIBRARY ROUTINES TO ACCESS THE PROCESS
%%


%OLD% %% Send an xsmap authorisation request to the process.
%OLD% %%
%OLD% -spec authz( pd_key(), fqn() ) -> ok.
%OLD% %%
%OLD% authz( PDKey, FQN ) ->
%OLD% 		arpa2_authz_xsmap ! { authz_xsmap, self(), PDKey, FQN },
%OLD% 		receive
%OLD% 		ok ->
%OLD% 			io:write( "Hoera!" ),
%OLD% 			io:nl();
%OLD% 		Reply ->
%OLD% 			io:write( "..." ),
%OLD% 			io:write( Reply ),
%OLD% 			io:nl()
%OLD% 		end.


%%
%% DATABASE TABLE INTERACTIONS
%%



%% 
%% ResourceTab
%% -----------------------------+-----------------------------
%% { ResUUID, CurDom }          | [ {LineNo,Level,[AdrSel]} ]
%%
%% Resources are located with a binary ResUUID and a CurDom.
%%
%% Resources map to a list of ACLs, each at a given Level and
%% each consisting of any number of AdrSel entries.  The first
%% ACL to match determines the Level that is reported.
%%

-spec resource_pre( dom(),uuid() ) -> {dom(),uuid()}.
resource_pre( CurDom,ResUUID ) ->
		{ ResUUID,CurDom }.

-spec resource_post( dom(),{lineno(),level(),acl()} ) -> {lineno(),level(),acl()}.
resource_post( _CurDom,Value ) ->
		case Value of
		{_LineNo,_Level,_AdrSels} -> Value
		end.

-spec resource( db(),dom(),uuid(),accessibility() ) -> accessibility().
resource( Db,CurDom,ResUUID,Accu ) ->
		PostProc = fun( Elem,ElemAccu ) ->
			%TODO% Really?  How to foldl() on one resource?!?
			io:format( "Prefixing ~p~n", [resource_post(CurDom,Elem)]),
			[ resource_post( CurDom,Elem ) | ElemAccu ]
		end,
		Key = resource_pre( CurDom,ResUUID ),
		case Key of
		nokey ->
			Values = [];
		_ ->
			case ets:lookup( maps:get( resourceTab,Db ),Key ) of
			[{Key,Values}] ->
				true;
			_ ->
				Values = []
			end
		end,
		lists:foldr( PostProc,Accu,Values ).

%% 
%% CommunicationTab
%% -----------------------------+-------------------------------
%% { Uid, Lab, CurDom }         | [ {LineNo, Level, [AdrSel]} ]
%%
%% Communication is pinned on the {Uid,Lab,CurDom} combo of an
%% identityHubUser/Group/Role with whom communication is attempted.
%%
%% Communication maps to a list of ACLs, each at a given Level
%% and each consisting of any number of AdrSel entries.  The
%% first ACL to match determines the Level that is reported.
%% 

-spec communication_pre( dom(),adr() ) -> adr().
communication_pre( CurDom,Adr ) ->
		case Adr of
		{Uid,Lab,current}    -> {Uid,Lab,CurDom};
		% {_Uid,_Lab,CurDom} -> Adr; % Covered below
		_                    -> Adr
		end.

-spec communication_post( dom(),{lineno(),level(),acl()} ) -> {lineno(),level(),acl()}.
communication_post( _CurDom,Value ) ->
		Value.

-spec communication( db(),dom(),adr(),accessibility() ) -> accessibility().
communication( Db,CurDom,Adr,Accu ) ->
		PostProc = fun( Elem,ElemAccu ) ->
			%TODO% Really?  How to foldl() on one communication?!?
			[ communication_post( CurDom,Elem ) | ElemAccu ]
		end,
		Key = communication_pre( CurDom,Adr ),
		case Key of
		nokey ->
			Values = [];
		_ ->
			case ets:lookup( maps:get( communicationTab,Db ),Key ) of
			[{Key,Values}] ->
				true;
			_ ->
				Values = []
			end
		end,
		lists:foldr( PostProc,Accu,Values ).

