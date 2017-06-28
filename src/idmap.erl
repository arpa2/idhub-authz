%% Identity Mapper for ARPA2 IdentityHub Authorisation
%%
%% This process answers "As what may User A pose?"
%%
%% From: Rick van Rein <rick@openfortress.nl>


-module(idmap).

-export([
	init/0,
	%OLD% idmap/1,
	%OLD% map_idkind/0,
	%OLD% pd_insert/2,
	%OLD% pd_fetch/1,
	impose/3, impose/4,
	access/3
	]).

-include("donai.hrl").

%% The database is a collection of ETS tables that will be updated to
%% accommodate the idmap and xsmap setup.  There may be updates to the
%% database in order to commit changes.
-type db() :: #{}.


%% The 'idmap' database is a disc-stored cache that unfolds
%% the inheritance of ARPA2 identities.  The mapping is keyed
%% by an externally authenticated identity and lists the
%% corresponding authorised identities.
%%
%% The storage structure uses shared data wherever possible.
%% This is done by reflecting the inheritance hierarchy within
%% a domain -- but crossover is not done, not even for a
%% foreign user welcomed into the domain.  This requires that
%% no cyclic dependencies exist (though we might handle that
%% in the future too).
%%
%% The data is stored in the process dictionary, so it ends up
%% in memory.  This is possible because
%%   - it is temporary data, with reference data on disc
%%   - memory is cheap and distribution scales linearly
%%
%% When starting up, the entire structure must be built from
%% scratch.  This would be awful, if it wasn't for the cluster
%% that allows nodes to go offline individually.  Spread your
%% cluster geographically and you should not have incredible
%% downtime, and when power surges do occur the rebuild is not
%% likely to be the slowest part of the offline period.
%%
%% When we add cluster communication to notify availability
%% of domains on nodes, we may want to retract service before
%% registering, and only resurrect domain service after having
%% set them up.  Until a domain is resurrected, it will be
%% handled by other nodes in the cluster.
%%
%% This design is optimised for incremental updates; as arrows
%% are added and withdrawn from the inheritance diagram we
%% should modify the data.


%OLD% %% Identities can be of various kinds, and some infrastructure
%OLD% %% is useful to distinguish them.
%OLD% %%TODO%% We experimentally sort mapped identities on their kind.
%OLD% %%
%OLD% -define( user,      0 ).
%OLD% -define( pseudonym, 1 ).
%OLD% -define( alias,     2 ).
%OLD% -define( member,    3 ).
%OLD% -define( group,     4 ).
%OLD% -define( occupant,  5 ).
%OLD% -define( role,      6 ).
%OLD% %%
%OLD% -define( minKind,   0 ).
%OLD% -define( maxKind,   6 ).
%OLD% %%

-type idkind() :: user | pseudonym | alias |
                      member | group | occupant | role.

%OLD% %% A translation table for acceptable strings describing kinds of identity
%OLD% %%
%OLD% map_idkind() -> #{
%OLD% 	"user"      => ?user,
%OLD% 	"pseudonym" => ?pseudonym,
%OLD% 	"alias"     => ?alias,
%OLD% 	"member"    => ?member,
%OLD% 	"group"     => ?group,
%OLD% 	"occupant"  => ?occupant,
%OLD% 	"role"      => ?role }.


%OLD% %% Key and val for the process dictionary
%OLD% %% This involves sharing data from the inheritance tree
%OLD% %% TODO: For now, the process dictionary values are much
%OLD% %%       simpler, capturing just the parsed adr() for the
%OLD% %%       entry, along with its inherited identities.
%OLD% %%
%OLD% -type pd_key() :: fqn().
%OLD% %%
%OLD% %TODO%FUTURE% -type pd_val() :: [ { adr(),  pd_val() } ].
%OLD% -type pd_val() :: { adr(), idkind(), [ fqn() ] }.



%%
%% Database management
%%


%% Initialise by clearing the process dictionary
%%
-spec init() -> none().
%%
init () -> erase ().

%OLD% %% Map an identity and flatten the result into a list
%OLD% %%
%OLD% -spec idmap( pd_key() ) -> [ adr() ].
%OLD% %%
%OLD% %%LATER%PERHAPS% idmap( authn_id ) -> idmap_flatten( get(id) ).
%OLD% idmap( authn_id ) -> merge( get( authn_id )).

%OLD% %% Flatten a nested structure identity hierarchy
%OLD% %% TODO: For now, the process dictionary values are much
%OLD% %%       simpler, capturing just the parsed adr() for the
%OLD% %%       entry, along with its inherited identities.
%OLD% %%
%OLD% -spec flatten( pd_val() ) -> [ {idkind(),adr()} ].
%OLD% %%
%OLD% %LATER%PERHAPS% idmap_flatten( PdVal ) -> idmap_flatten( [], PdVal ).
%OLD% %%
%OLD% flatten( {MyAdr,MyKind,Inherits} ) ->
%OLD% 		[ {MyKind,MyAdr} | lists:map( flatten,
%OLD% 					lists:map( get,Inherits )) ].


%OLD% %% Merge multiple lists of {idkind(), adr()} on dropping kind,
%OLD% %% Assuming the merged lists are already listed on dropping kind.
%OLD% %%
%OLD% -spec merge ( [pd_val()] ) -> [adr()].
%OLD% %%
%OLD% merge( IdLists ) -> merge( IdLists,?minKind,[],[] ).
%OLD% %%
%OLD% -spec merge ( [{idkind(),adr()}], idkind(), [adr()], [{idkind(),adr()}] ) -> [adr()].
%OLD% %%
%OLD% merge( [],_,Accu,[] ) -> Accu;
%OLD% merge( [],?maxKind,Accu,Future ) -> Future ++ Accu;
%OLD% merge( [],Kind,Accu,Future ) -> merge( Future,Kind+1,Accu,[] );
%OLD% merge( [{Kind,Adr}|More],Kind,Accu,Future ) ->
%OLD% 		merge( More,Kind,[Adr|Accu],Future );
%OLD% merge( [FutureId|More],Kind,Accu,Future ) ->
%OLD% 		merge( More,Kind,Accu,[FutureId|Future] ).

%OLD% %% Insert an item in the process dictionary
%OLD% %%
%OLD% -spec pd_insert( pd_key(),pd_val() ) -> pd_val().
%OLD% %%
%OLD% pd_insert( Key, Val ) ->
%OLD% 		put( Key, Val ),
%OLD% 		ok.

%OLD% %% Bluntly dump all the direct and indirect identities, regardless of kind
%OLD% %%
%OLD% -spec dump_all( pd_val() ) -> [ {adr(),idkind()} ].
%OLD% %%
%OLD% dump_all( {Adr,Kind,Inherit} ) ->
%OLD% 		dump_all( Adr,Kind,Inherit,[] ).
%OLD% %%
%OLD% -spec dump_all( adr(),idkind(),[pd_key()],[adr()] ) -> [ {adr(),idkind()} ].
%OLD% %%
%OLD% %FOLDL_COVERED% dump_all( Adr,_Kind,[],Accu ) -> [ Adr | Accu ];
%OLD% dump_all( Adr,Kind,Inherit,Accu ) ->
%OLD% 		% case Inherit of
%OLD% 		% [] -> [ Adr ];
%OLD% 		% [ Adr | lists:map( dump_all, lists:map( get,Inherit )) ].
%OLD% 		lists:foldl(
%OLD% 			fun( SubKey,SubAccu ) ->
%OLD% 				{ SubAdr,SubKind,SubInherit } = get (SubKey),
%OLD% 				dump_all( SubAdr,SubKind,SubInherit,SubAccu )
%OLD% 			end,
%OLD% 			[ {Adr,Kind} | Accu ],
%OLD% 			Inherit ).
%OLD% 		% [ Adr | lists:map( fun(Key) -> pd_fetch( Key ) end,Inherit ) ].
%OLD% 
%%

%TODO% -type kindid() :: { idkind(),adr() }.
%TODO% 
%TODO% -spec merge( pd_key() ) -> [ adr() ].
%TODO% merge( Key ) ->
%TODO% 		SelectId = fun( {_Kind,Id} ) -> Id end,
%TODO% 		lists:map( SelectId,merge( Key,[],[] )).
%TODO% 
%TODO% -spec merge( pd_key(),[kindid()],[kindid()] ) -> [kindid()].
%TODO% 
%TODO% merge( Key,Accu,L8er ) ->
%TODO% 		{ Id,Kind,Sub } = get( Key ),
%TODO% 		HasSameKind = fun( {SelKind,_Id} ) -> SelKind == Kind end,
%TODO% 		{ SubNow,SubL8er } = lists:splitwith( HasSameKind,Sub },
%TODO% 		TODO.
		

%OLD% %% Lookup an item from the process dictionary
%OLD% %%
%OLD% -spec pd_fetch( pd_key() ) -> [ adr() ].
%OLD% %%
%OLD% %%TODO%FUTURE% pd_fetch( Key ) -> idmap_flatten( get(Key) ).
%OLD% pd_fetch( Key ) ->
%OLD% 		io:format( "Key = ~p~n", [Key] ),
%OLD% 		Val = get(Key),
%OLD% 		io:format( "Val = ~p~n", [Val] ),
%OLD% 		% Flattened = flatten( Val ),
%OLD% 		% io:write( Flattened ),
%OLD% 		% merge( Flattened ).
%OLD% 		Dumped = dump_all( Val ),
%OLD% 		io:format( "Dumped = ~p~n", [Dumped] ),
%OLD% 		Merged = merge( Dumped ),
%OLD% 		io:format( "Merged = ~p~n", [Merged] ),
%OLD% 		Merged.
%OLD% 		% io:write( Dumped ),
%OLD% 		% io:nl(),
%OLD% 		% io:nl(),
%OLD% 		% merge( Dumped ).


%%
%% EXPERIMENTAL NEW CODE
%%

%TODO_NAH_NOT_LIKE_THIS% %% Find pseudonyms for a given address under the current domain.
%TODO_NAH_NOT_LIKE_THIS% %% This will return at least the original address, possibly others.
%TODO_NAH_NOT_LIKE_THIS% %% Note that only a basic form, without label, would find anything.
%TODO_NAH_NOT_LIKE_THIS% %%
%TODO_NAH_NOT_LIKE_THIS% %% Local identities have keys like {Usr,CurDom}
%TODO_NAH_NOT_LIKE_THIS% %% Remote identities have keys like {Usr,Lab|absent,Dom,CurDom}
%TODO_NAH_NOT_LIKE_THIS% %%
%TODO_NAH_NOT_LIKE_THIS% -spec pseudonym( dom(),adr() ) -> [ adr() ].
%TODO_NAH_NOT_LIKE_THIS% %%
%TODO_NAH_NOT_LIKE_THIS% pseudonyms( CurDom,[{Usr,Lab,Dom}] ) ->
%TODO_NAH_NOT_LIKE_THIS% 		if Lab == absent andalso Dom == CurDom ->
%TODO_NAH_NOT_LIKE_THIS% 			SearchKey = {Usr,CurDom};
%TODO_NAH_NOT_LIKE_THIS% 		Dom != CurDom ->
%TODO_NAH_NOT_LIKE_THIS% 			SearchKey = {Usr,Lab,Dom,CurDom};
%TODO_NAH_NOT_LIKE_THIS% 		true ->
%TODO_NAH_NOT_LIKE_THIS% 			SearchKey = {}
%TODO_NAH_NOT_LIKE_THIS% 		end,
%TODO_NAH_NOT_LIKE_THIS% 		case SearchKey of
%TODO_NAH_NOT_LIKE_THIS% 		{} ->
%TODO_NAH_NOT_LIKE_THIS% 			Pseudo = [];
%TODO_NAH_NOT_LIKE_THIS% 		_ ->
%TODO_NAH_NOT_LIKE_THIS% 			Found = ets:lookup( PseudoTab,SearchKey ),
%TODO_NAH_NOT_LIKE_THIS% 			Pseudo = [ {Usr,Lab,current} | {Usr,Lab} <- Found }
%TODO_NAH_NOT_LIKE_THIS% 		end;
%TODO_NAH_NOT_LIKE_THIS% 		[ Adr | Pseudo ].
%TODO_NAH_NOT_LIKE_THIS% 
%TODO_NAH_NOT_LIKE_THIS% 
%TODO_NAH_NOT_LIKE_THIS% %% Find all aliases for the given addresses under the current domain.
%TODO_NAH_NOT_LIKE_THIS% %% This will only work for base names, so without label, and only when
%TODO_NAH_NOT_LIKE_THIS% %% they reside under the local domain.
%TODO_NAH_NOT_LIKE_THIS% %%
%TODO_NAH_NOT_LIKE_THIS% -spec aliases( dom(),[adr()] ) -> [ adr() ].
%TODO_NAH_NOT_LIKE_THIS% %%
%TODO_NAH_NOT_LIKE_THIS% aliases( CurDom,Adrs ) ->
%TODO_NAH_NOT_LIKE_THIS% 		aliases( CurDom,Adrs,[] ).
%TODO_NAH_NOT_LIKE_THIS% %%
%TODO_NAH_NOT_LIKE_THIS% -spec aliases( dom(),[adr()],[adr()] ) -> [ adr() ].
%TODO_NAH_NOT_LIKE_THIS% %%
%TODO_NAH_NOT_LIKE_THIS% aliases( CurDom,[],Accu ) ->
%TODO_NAH_NOT_LIKE_THIS% 		Accu;
%TODO_NAH_NOT_LIKE_THIS% aliases( CurDom,Adrs,Accu ) ->
%TODO_NAH_NOT_LIKE_THIS% 		TODO = AwfulCode,
%TODO_NAH_NOT_LIKE_THIS% 		PrefixAliases = fun( Adr,Accu ) ->
%TODO_NAH_NOT_LIKE_THIS% 			NewAccu = [ RemoteAdr | Accu ],
%TODO_NAH_NOT_LIKE_THIS% 			case Adr of
%TODO_NAH_NOT_LIKE_THIS% 			{Usr,Lab,CurDom} ->
%TODO_NAH_NOT_LIKE_THIS% 				SearchKey = {Usr,Lab},
%TODO_NAH_NOT_LIKE_THIS% 				Found = ets:lookup( AliasTab,SearchKey ),
%TODO_NAH_NOT_LIKE_THIS% 				lists:foldl (PrePrefixAliases,NewAccu,Found);
%TODO_NAH_NOT_LIKE_THIS% 			RemoteAdr ->
%TODO_NAH_NOT_LIKE_THIS% 				NewAccu
%TODO_NAH_NOT_LIKE_THIS% 			end,
%TODO_NAH_NOT_LIKE_THIS% 			case Adr of
%TODO_NAH_NOT_LIKE_THIS% 			{Usr,absent} ->
%TODO_NAH_NOT_LIKE_THIS% 				[ {Usr,absent,CurDom} | Accu ];
%TODO_NAH_NOT_LIKE_THIS% 			{
%TODO_NAH_NOT_LIKE_THIS% 		lists:foldl( PrefixAliases,Accu,Adrs ).


%OLD% unfold_stage( StageName,CurDom,Adrs ) ->
%OLD% 		case StageName of
%OLD% 		pseudonyms ->
%OLD% 			StageFun = fun( Adr,Accu ) ->
%OLD% 				idmap:pseudonyms( CurDom,Adr,Accu ) end;
%OLD% 		aliases ->
%OLD% 			StageFun = fun( Adr,Accu ) ->
%OLD% 				idmap:aliases( CurDom,Adr,Accu ) end
%OLD% 		end,
%OLD% 		NewAccu = [],
%OLD% 		lists:foldl( StageFun,NewAccu,Adrs ).


%OLD% unfold_stages( StageNames,CurDom,Adr ) ->
%OLD% 		Unfold = fun( StageName,Accu ) ->
%OLD% 			unfold_stage( StageName,CurDom,Accu )
%OLD% 		end,
%OLD% 		InitAccu = [Adr],
%OLD% 		lists:foldl( Unfold,InitAccu,StageNames ).



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
		io:format( "Pseudonym Accu   ~p~n", [Accu]),
		PostProc = fun( Elem,ElemAccu ) ->
			io:format( "Pseudonum PostProc Elem ~p~n", [Elem]),
			io:format( "Pseudonum PostProc Post ~p~n", [pseudonym_post( CurDom,Elem )]),
			[ pseudonym_post( CurDom,Elem ) | ElemAccu ]
		end,
		Key = pseudonym_pre( CurDom,Adr ),
		io:format( "Pseudonym Key    ~p~n", [Key]),
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
		io:format( "Pseudonym Values ~p~n", [Values]),
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
			io:format( "New alias Elem ~p~n",[Elem] ),
			io:format( "Deliverd as    ~p~n",[alias_post( CurDom,Uid,Elem )] ),
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
		io:format( "Alias Key    ~p~n", [Key   ]),
		io:format( "Alias Values ~p~n", [Values]),
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
%% defaults to CurDom (also in case of a remote).
%TODO% Can we ever define an output Dom that is not CurDom?
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

%
% The following code appears to be *too functional* as far as Erlang thinks.
% The problem appears to be returning a function as a result, thereby relying
% on a context that is being closed down.  This has not been gone into deeply.
%
% The idea of this code was to specify all the table wrapper functions in a
% generic manner, and locate variation in the _pre and _post functions alone.
%
%TOO_FUNCTIONAL% %TODO% -spec pre_lookup_post( db(),fun(),fun() ) -> (dom(),adr(),[ adr() ] ) -> [ adr() ].
%TOO_FUNCTIONAL% 
%TOO_FUNCTIONAL% pre_lookup_post( Db,{TabName,Pre,Post} ) ->
%TOO_FUNCTIONAL% 	Table = maps:get( TabName,Db ),
%TOO_FUNCTIONAL% 	PostProc = fun( Elem,ElemAccu ) ->
%TOO_FUNCTIONAL% 		[ Post( CurDom,Elem ) | ElemAccu ]
%TOO_FUNCTIONAL% 	end,
%TOO_FUNCTIONAL% 	fun ( CurDom,Adr,Accu ) ->
%TOO_FUNCTIONAL% 		Key = Pre( CurDom,Adr ),
%TOO_FUNCTIONAL% 		case Key of
%TOO_FUNCTIONAL% 		nokey ->
%TOO_FUNCTIONAL% 			% This does not actually come out of flock_pre/2
%TOO_FUNCTIONAL% 			Values = [];
%TOO_FUNCTIONAL% 		_ ->
%TOO_FUNCTIONAL% 			Values = ets:lookup( Table, Key )
%TOO_FUNCTIONAL% 		end,
%TOO_FUNCTIONAL% 		lists:foldl( PostProc,Accu,Values ).
%TOO_FUNCTIONAL% 
%TOO_FUNCTIONAL% pre_lookup_post( Db ) ->
%TOO_FUNCTIONAL% 	Collective = [
%TOO_FUNCTIONAL% 		{ pseudonym,{pseudonymTable,pseudonym_pre,pseudonym_post} },
%TOO_FUNCTIONAL% 		{ alias,    {aliasTable,    alias_pre,    alias_post    } },
%TOO_FUNCTIONAL% 		{ flock,    {flockTable,    flock_pre,    flock_post    } } ],
%TOO_FUNCTIONAL% 	Collect = fun( {Atom,Setup},Map ) ->
%TOO_FUNCTIONAL% 		maps:put( Atom,pre_lookup_post( Db,Setup ),Map )
%TOO_FUNCTIONAL% 	end,
%TOO_FUNCTIONAL% 	lists:foldl( Collect,#{},Collective ).

%%TODO%% ResourceTab,CommunicationTab should move to xsmap.erl
%% 
%% ResourceTab
%% -----------------------------+-----------------------------
%% { ResUUID, CurDom }          | [ { Level, [ AdrSel ] } ]
%%
%% Resources are located with a binary ResUUID and a CurDom.
%%
%% Resources map to a list of ACLs, each at a given Level and
%% each consisting of any number of AdrSel entries.  The first
%% ACL to match determines the Level that is reported.
%% 
%% CommunicationTab
%% -----------------------------+-----------------------------
%% { Uid, Lab, CurDom }         | [ { Level, [ AdrSel ] } ]
%%
%% Communication is located by the Uid, Lab and CurDom of a
%% user with whom communication is attempted.
%%
%% Communication maps to a list of ACLs, each at a given Level
%% and each consisting of any number of AdrSel entries.  The
%% first ACL to match determines the Level that is reported.
%% 

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
%% Authorisation commands.
%%
%%  1. impose/4 Asks if an authenticated user can authorise as another user.
%%     The target domain comes from the intended authorisation user's domain.
%%     The mappings traversed for this call are pseudonym and alias.
%%
%%  2. access/2 Asks for the identities accessible to an authenticated user.
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
impose( Db,CurDom,AuthnId,AuthzReqId ) ->
		lists:member( AuthzReqId,impose( Db,CurDom,AuthnId )).

%% Return the list of authorisation identities available for a given
%% authenticated identity AuthnId.
%%
-spec impose( db(),dom(),adr() ) -> [ adr() ].
impose( Db,CurDom,AuthnId ) ->
		io:format ("~n", []),
		%TODO% Following should be a library routine in donai
		Accu0 = case AuthnId of
		{Uid,Lab,CurDom} -> [ {Uid,Lab,current} ];
		_ -> [ AuthnId ]
		end,
		io:format( "Accu0 ~p~n",[Accu0] ),
		Accu1 = pseudonym( Db,CurDom,AuthnId,Accu0 ),
		io:format( "Accu1 ~p~n",[Accu1] ),
		AliasElem = fun( AliasAdr,AliasAccu ) ->
			alias( Db,CurDom,AliasAdr,AliasAccu )
		end,
		Accu2 = lists:foldl( AliasElem,Accu1,Accu1 ),
		io:format( "Accu2 ~p~n",[Accu2] ),
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

