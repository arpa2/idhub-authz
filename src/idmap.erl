%% Identity Mapper for ARPA2 IdentityHub Authorisation
%%
%% This process answers "As what may User A pose?"
%%
%% From: Rick van Rein <rick@openfortress.nl>


-module( idmap ).

-export([
	init/0,
	map_idkind/0,
	impose/3, impose/4,
	access/3
	]).

-include( "donai.hrl" ).
-include( "idmap.hrl" ).

%% A translation table for acceptable strings describing kinds of identity
%%
map_idkind() -> #{
	"user"      => ?user,
	"pseudonym" => ?pseudonym,
	"alias"     => ?alias,
	"member"    => ?member,
	"group"     => ?group,
	"occupant"  => ?occupant,
	"role"      => ?role
}.



%%
%% MODULE MANAGEMENT
%%


%% Initialise by clearing the process dictionary
%%
-spec init() -> none().
%%
init () -> 0.


%%
%% DATABASE MANAGEMENT
%%


%%
%% The following tables are assumed:
%% 
%% PseudonymTab
%% -----------------------------+-----------------------------
%% { Uid, CurDom }              | [ Uid |
%% | { { Uid, Lab, Dom },       |   { Uid, Dom } ],
%%       CurDom }               |
%%
%% Pseudonyms are defined for local users without label under
%% CurDom; in addition, for remote users that may have (what we
%% would call) a label and reside under another domein.
%%
%% Pseudonyms have a Uid and a Dom that defaults to CurDom.
%%

%% Pre-process for pseudonymTab lookup.  Used in pseudonym/4 below.
%% Given a login identity or a remote address, find its pseudonyms.
%% The input address Adr may represent the current domain CurDom
%% briefly as current, which will be changed for proper lookup.
%%
%% Not all identities work to find pseudonyms; local identities
%% only work when they have no label, but remote identities may
%% not be interpreted as having a label, so for them it is fine.
%% Remote identities are marked with special keys.  When lookup
%% in pseudonymTab is senseless, the value nokey is returned.
%%
-spec pseudonym_pre( dom(),adr() ) -> {uid(),dom()} | {adr(),dom()} | nokey.
pseudonym_pre( CurDom,Adr ) ->
		case Adr of
		{Uid,absent,current}  -> {Uid,CurDom};
		{Uid,absent,CurDom}   -> {Uid,CurDom};
		{_Uid,_Lab,current}   -> nokey;
		{_Uid,_Lab,CurDom}    -> nokey;
		{_Uid,_Lab,_OtherDom} -> {Adr,CurDom};
		_                     -> nokey
		end.

%% Post-process a value found in pseudonymTab.  Used in pseudonym/4 below.
%% Any use of the current domain CurDom is replaced by shorthand current.
%%
-spec pseudonym_post( dom(), {uid(),dom()} | uid() ) -> [adr()].
pseudonym_post( CurDom,Value ) ->
		case Value of
		{Uid,CurDom} -> {Uid,absent,current};
		{Uid,Dom}    -> {Uid,absent,Dom};
		Uid          -> {Uid,absent,current}
		end.

%% Map an input address Adr into a list of pseudonyms.  The result does not
%% include Adr, but the accumulutor Accu can be used by the caller to add it
%% indepenedently.  Input and output addresses may use current to abbreviate
%% the current domain CurDom.
%%
%% Not all addresses Adr can have pseudonyms, and the outcome may be an
%% empty list (meaning that Accu is returned without prefixing options).
%%
-spec pseudonym( db(),dom(),adr(),[adr()] ) -> [adr()].
pseudonym( Db,CurDom,Adr,Accu ) ->
		PostProc = fun( Elem,ElemAccu ) ->
			[ pseudonym_post( CurDom,Elem ) | ElemAccu ]
		end,
		Key = pseudonym_pre( CurDom,Adr ),
		case Key of
		nokey ->
			Values = [];
		_ ->
			case ets:lookup( maps:get( pseudonymTab,Db ),Key ) of
			[{Key,Values}] ->
				true;
			[] ->
				Values = []
			end
		end,
		lists:foldl( PostProc,Accu,Values ).

%%
%% AliasTab
%% -----------------------------+-----------------------------
%% { Uid, CurDom }              | [ Lab |
%%                              |   { Lab, Dom } ]
%%
%% Aliases are only defined for local users without label.
%%
%% Aliases have a Lab and a Dom that defaults to CurDom.
%%

%% Pre-processing of aliasTab entries.  Part of alias/4 below.
%% Only return a key for aliasTab processing for Adr identities
%% without a label under the current domain CurDom.  Accept
%% current as domain in Adr, expand to CurDom in the key.
%%
-spec alias_pre( dom(),adr() ) -> {uid(),dom()} | nokey.
alias_pre( CurDom,Adr ) ->
		case Adr of
		{Uid,absent,current} -> {Uid,CurDom};
		{Uid,absent,CurDom}  -> {Uid,CurDom};
		_                    -> nokey
		end.

%% Post-processing of aliasTab entries.  Part of alias/4 below.
%% Returned addresses under the current domain CurDom are mapped
%% to the current shorthand, which is also returned for entries
%% that assume CurDom by not mentioning a domain at all.  Note
%% that entries do not mention the Uid, because that is part of
%% the looked up key already.
%%
-spec alias_post( dom(),uid(),[ lab() | {lab(),dom()} ] ) -> [ adr() ].
alias_post( CurDom,Uid,Value ) ->
		case Value of
		{Lab,CurDom} -> {Uid,Lab,current};
		{Lab,Dom}    -> {Uid,Lab,Dom};
		Lab          -> {Uid,Lab,current}
		end.

%% Expand identities according to aliasTab entries.  Permit the
%% input address Adr with domain set to current to indicate the
%% current domain CurDom, and return this when applicable, but
%% stay open to remote Adr in both input and output.
%%
%% Only addresses Adr without label part can have aliases, so
%% login users and pseudonyms, but not aliases, group members
%% or role occupants, nor remote users.  None of these forms,
%% with or without aliases, are repeated in the output; but
%% there is an accumulator Accu to which answers are prefixed.
%%
-spec alias( db(),dom(),adr(),[ adr() ] ) -> [ adr() ].
alias( Db,CurDom,Adr,Accu ) ->
		{Uid,_,_} = Adr,
		PostProc = fun( Elem,ElemAccu ) ->
			[ alias_post( CurDom,Uid,Elem ) | ElemAccu ]
		end,
		Key = alias_pre( CurDom,Adr ),
		case Key of
		nokey ->
			Values = [];
		_ ->
			case ets:lookup( maps:get( aliasTab,Db ), Key ) of
			[{Key,Values}] ->
				true;
			[] ->
				Values = []
			end
		end,
		lists:foldl( PostProc,Accu,Values ).

%%
%% FlockTab
%% -----------------------------+-----------------------------
%% { Uid, Lab, CurDom }         | [ { Kind, Uid, Lab } |
%% | { { Uid, Lab, Dom },       |   { Kind, Uid, Lab, Dom } ]
%%     CurDom }                 |
%%
%% Flocks may start from a user (including alias and pseudonym)
%% or, when recursion is supported, from a group Kind combined
%% with Uid, non-empty Lab and CurDom.  Alternatively, they may
%% start from a remote identity that is ackowledged by CurDom.
%% Note: Input Kind is not part of the Key (assumed known).
%%
%% Flocks may combine to a role or group Kind, defining a Uid
%% for that Kind, along with a Lab for the user and a Dom that
%% defaults to CurDom (also in case of a remote).  Note that
%% the variant with a domain in the output is used to invite
%% remote users into groups and roles.
%%

%% Pre-processing for flockTab lookup.  Used in flock/4 below.
%% This replaces any occurrence of domain current in the input address,
%% so it can be mapped.  It also prepares different forms for local and
%% remote addresses.
%%
-spec flock_pre( dom(),adr() | {adr(),dom()} ) -> [ {idkind(),uid(),lab()} | {idkind(),uid(),lab(),dom()} ].
flock_pre( CurDom,Adr ) ->
		case Adr of
		{Uid,Lab,current}     -> {Uid,Lab,CurDom};
		{_Uid,_Lab,CurDom}    -> Adr;
		{_Uid,_Lab,_OtherDom} -> {Adr,CurDom}
		end.

%% Post-processing for flockTab lookup.  Used in flock/4 below.
%% This appends the current domain CurDom as current when it is absent or
%% explicitly set to CurDom.  Other domains Dom are passed literally.
%%
-spec flock_post( dom(), {idkind(),uid(),lab()} | {idkind(),uid(),lab(),dom()} ) -> adr().
flock_post( CurDom,Value ) ->
		case Value of
		{_Kind,Uid,Lab}        -> {Uid,Lab,current};
		{_Kind,Uid,Lab,CurDom} -> {Uid,Lab,current};
		{_Kind,Uid,Lab,Dom}    -> {Uid,Lab,Dom}
		end.

%% Apply the flockTab table to determine occurrence of address Adr as
%% role occupant, or as group member.  This does not repeat the input
%% value, though the caller may use accumulater Accu for that purpose.
%% The input Adr as well as returned addresses may use current as their
%% domain as a shorthand for CurDom.
%%
%% This table is usually applied
%%  - once to learn roles and groups for a given user
%%  - for any of the new groups found, another time to learn its roles
%%  - TODO:FUTURE: repeat the previous for any new groups found
%%
-spec flock( db(),dom(),adr(),[ adr() ] ) -> [ adr() ].
flock( Db,CurDom,Adr,Accu ) ->
		PostProc = fun( Elem,ElemAccu ) ->
			[ flock_post( CurDom,Elem ) | ElemAccu ]
		end,
		Key = flock_pre( CurDom,Adr ),
		case Key of
		nokey ->
			% This does not actually come out of flock_pre/2
			Values = [];
		_ ->
			case ets:lookup( maps:get( flockTab,Db ), Key ) of
			[{Key,Values}] ->
				true;
			[] ->
				Values = []
			end
		end,
		lists:foldl( PostProc,Accu,Values ).

%%
%% Resolution requests take the form command,value,...
%% where the command is an atom() that is recognised as a
%% command, and value counts as input, such as a tableref.
%%
%% The initial Adr and target information such as CurDom
%% are communicated separately, along with the kind of
%% request that is being made.
%%


%%
%% AUTHORISATION INQUISITION FUNCTIONS
%%


%%
%% We recognise two types of command:
%%
%%  1. impose/4 Asks if an authenticated user can authorise as another user.
%%     The target domain comes from the intended authorisation user's domain.
%%     The mappings traversed for this call are pseudonym and alias.  Note
%%     that it internally uses impose/3 which lists possible identities.
%%
%%  2. access/3 Asks for the identities accessible to an authenticated user.
%%     The target domain is provided independently, and will be determined
%%     from the targeted resource (or its instance) or the party to whom
%%     communication is being attempted (this level of detail is located in
%%     the xsmap module).  The mappings traversed for this call are alias when
%%     not already supplied, then flock is repeated as often as is needed.
%%     TODO: flock does not repeat more than twice for now; groups recursion
%%     is a future extension, so we first find groups and roles, then roles for
%%     the groups.  Names returned only include member names and role
%%     occupants, but not logins, pseudonyms or aliases.
%%     TODO: Split access into resources and comms?  Probably not, the user
%%     can always manipulate which of his identities are visible where.
%%     Maybe later, though, not completely sure yet.
%%
%% Since authorisation is done for a given domain CurDom, addresses may at
%% any time use current as their domain name, both on input and output.
%%

%% Check whether authenticated identity AuthnId can impose as authorisation
%% identity AuthzReqId.  This is often desired when a protocol learns about
%% specific identity claims by the user, for instance in SMTP's MAIL FROM
%% command.  Usually, the authorisation identity will henceforth be used
%% instead of the authenticated identity, as this too can be considered an
%% authenticated identity.  Note how the privileges may degrade due to this,
%% but also that the preselection greatly helps in selection how one looks
%% and, not unimportantly, how fast this can be proven.  Note that in some
%% cases a sequence of such operations may be desired, such as when the
%% SMTP command DATA passes a From: header.  Note how this prepares for
%% communication access checks later on.
%%
-spec impose( db(),dom(),adr(),adr() ) -> true | false.
impose( _,_CurDom,AuthnId,AuthnId ) ->
		%TODO% Need to check CurDom in AuthnId?
		true;
impose( Db,CurDom,AuthnId,AuthzReqId ) ->
		lists:member( AuthzReqId,impose( Db,CurDom,AuthnId )).

%% Return the list of authorisation identities available for a given
%% authenticated identity AuthnId.
%%
-spec impose( db(),dom(),adr() ) -> [ adr() ].
impose( Db,CurDom,AuthnId ) ->
		%TODO% Following should be a library routine in donai
		Accu0 = case AuthnId of
		{Uid,Lab,CurDom} -> [ {Uid,Lab,current} ];
		_ -> [ AuthnId ]
		end,
		Accu1 = pseudonym( Db,CurDom,AuthnId,Accu0 ),
		AliasElem = fun( AliasAdr,AliasAccu ) ->
			alias( Db,CurDom,AliasAdr,AliasAccu )
		end,
		Accu2 = lists:foldl( AliasElem,Accu1,Accu1 ),
		Accu2.


%% Given an authenticated identity or authorisation identity AuthId, list
%% the authorisation identites available.  This is helpful in determining
%% two kinds of access as regulated by ACLs, namely access to a resource
%% or resource instance, and communication access to a user, both managed
%% under the current domain CurDom.
%%
%% TODO: We ignore the Kind of the entry, so we even repeat lookup on roles.
%%
-spec access( db(),dom(),adr() ) -> [ adr() ].
access( Db,CurDom,AuthId ) ->
		%TODO% Following should be a library routine in donai
		Accu0 = case AuthId of
		{Uid,Lab,CurDom} -> [ {Uid,Lab,current} ];
		_ -> [ AuthId ]
		end,
		Accu1 = alias( Db,CurDom,AuthId,Accu0 ),
		FlockElem = fun( Adr,FlockAccu ) ->
			flock( Db,CurDom,Adr,FlockAccu )
		end,
		Accu2 = lists:foldl( FlockElem,Accu1,Accu1 ),
		Accu3 = lists:foldl( FlockElem,Accu1,Accu2 ),
		Accu3.

