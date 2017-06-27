%% Identity Mapper for ARPA2 IdentityHub Authorisation
%%
%% This process answers "As what may User A pose?"
%%
%% From: Rick van Rein <rick@openfortress.nl>


-module(idmap).

-export([
	init/0,
	idmap/1,
	map_idkind/0,
	pd_insert/2,
	pd_fetch/1
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


%% Identities can be of various kinds, and some infrastructure
%% is useful to distinguish them.
%%TODO%% We experimentally sort mapped identities on their kind.
%%
-define( user,      0 ).
-define( pseudonym, 1 ).
-define( alias,     2 ).
-define( member,    3 ).
-define( group,     4 ).
-define( occupant,  5 ).
-define( role,      6 ).
%%
-define( minKind,   0 ).
-define( maxKind,   6 ).
%%
-type idkind() :: user | pseudonym | alias |
                      member | group | occupant | role.

%% A translation table for acceptable strings describing kinds of identity
%%
map_idkind() -> #{
	"user"      => ?user,
	"pseudonym" => ?pseudonym,
	"alias"     => ?alias,
	"member"    => ?member,
	"group"     => ?group,
	"occupant"  => ?occupant,
	"role"      => ?role }.


%% Key and val for the process dictionary
%% This involves sharing data from the inheritance tree
%% TODO: For now, the process dictionary values are much
%%       simpler, capturing just the parsed adr() for the
%%       entry, along with its inherited identities.
%%
-type pd_key() :: fqn().
%%
%TODO%FUTURE% -type pd_val() :: [ { adr(),  pd_val() } ].
-type pd_val() :: { adr(), idkind(), [ fqn() ] }.



%%
%% Database management
%%


%% Initialise by clearing the process dictionary
%%
-spec init() -> none().
%%
init () -> erase ().

%% Map an identity and flatten the result into a list
%%
-spec idmap( pd_key() ) -> [ adr() ].
%%
%%LATER%PERHAPS% idmap( authn_id ) -> idmap_flatten( get(id) ).
idmap( authn_id ) -> merge( get( authn_id )).
		

%% Flatten a nested structure identity hierarchy
%% TODO: For now, the process dictionary values are much
%%       simpler, capturing just the parsed adr() for the
%%       entry, along with its inherited identities.
%%
-spec flatten( pd_val() ) -> [ {idkind(),adr()} ].
%%
%LATER%PERHAPS% idmap_flatten( PdVal ) -> idmap_flatten( [], PdVal ).
%%
flatten( {MyAdr,MyKind,Inherits} ) ->
		[ {MyKind,MyAdr} | lists:map( flatten,
					lists:map( get,Inherits )) ].


%% Merge multiple lists of {idkind(), adr()} on dropping kind,
%% Assuming the merged lists are already listed on dropping kind.
%%
-spec merge ( [pd_val()] ) -> [adr()].
%%
merge( IdLists ) -> merge( IdLists,?minKind,[],[] ).
%%
-spec merge ( [{idkind(),adr()}], idkind(), [adr()], [{idkind(),adr()}] ) -> [adr()].
%%
merge( [],_,Accu,[] ) -> Accu;
merge( [],?maxKind,Accu,Future ) -> Future ++ Accu;
merge( [],Kind,Accu,Future ) -> merge( Future,Kind+1,Accu,[] );
merge( [{Kind,Adr}|More],Kind,Accu,Future ) ->
		merge( More,Kind,[Adr|Accu],Future );
merge( [FutureId|More],Kind,Accu,Future ) ->
		merge( More,Kind,Accu,[FutureId|Future] ).

%% Insert an item in the process dictionary
%%
-spec pd_insert( pd_key(),pd_val() ) -> pd_val().
%%
pd_insert( Key, Val ) ->
		put( Key, Val ),
		ok.

%% Bluntly dump all the direct and indirect identities, regardless of kind
%%
-spec dump_all( pd_val() ) -> [ {adr(),idkind()} ].
%%
dump_all( {Adr,Kind,Inherit} ) ->
		dump_all( Adr,Kind,Inherit,[] ).
%%
-spec dump_all( adr(),idkind(),[pd_key()],[adr()] ) -> [ {adr(),idkind()} ].
%%
%FOLDL_COVERED% dump_all( Adr,_Kind,[],Accu ) -> [ Adr | Accu ];
dump_all( Adr,Kind,Inherit,Accu ) ->
		% case Inherit of
		% [] -> [ Adr ];
		% [ Adr | lists:map( dump_all, lists:map( get,Inherit )) ].
		lists:foldl(
			fun( SubKey,SubAccu ) ->
				{ SubAdr,SubKind,SubInherit } = get (SubKey),
				dump_all( SubAdr,SubKind,SubInherit,SubAccu )
			end,
			[ {Adr,Kind} | Accu ],
			Inherit ).
		% [ Adr | lists:map( fun(Key) -> pd_fetch( Key ) end,Inherit ) ].

%%

-type kindid() :: { idkind(),adr() }.

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
		

%% Lookup an item from the process dictionary
%%
-spec pd_fetch( pd_key() ) -> [ adr() ].
%%
%%TODO%FUTURE% pd_fetch( Key ) -> idmap_flatten( get(Key) ).
pd_fetch( Key ) ->
		io:format( "Key = ~w~n", [Key] ),
		Val = get(Key),
		io:format( "Val = ~w~n", [Val] ),
		% Flattened = flatten( Val ),
		% io:write( Flattened ),
		% merge( Flattened ).
		Dumped = dump_all( Val ),
		io:format( "Dumped = ~w~n", [Dumped] ),
		Merged = merge( Dumped ),
		io:format( "Merged = ~w~n", [Merged] ),
		Merged.
		% io:write( Dumped ),
		% io:nl(),
		% io:nl(),
		% merge( Dumped ).


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


unfold_stage( StageName,CurDom,Adrs ) ->
		case StageName of
		pseudonyms ->
			StageFun = fun( Adr,Accu ) ->
				idmap:pseudonyms( CurDom,Adr,Accu ) end;
		aliases ->
			StageFun = fun( Adr,Accu ) ->
				idmap:aliases( CurDom,Adr,Accu ) end
		end,
		NewAccu = [],
		lists:foldl( StageFun,NewAccu,Adrs ).


unfold_stages( StageNames,CurDom,Adr ) ->
		Unfold = fun( StageName,Accu ) ->
			unfold_stage( StageName,CurDom,Accu )
		end,
		InitAccu = [Adr],
		lists:foldl( Unfold,InitAccu,StageNames ).



%%
%% The following tables are assumed:
%% 
%% PseudoTab
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

-spec pseudonym_pre( dom(),adr() ) -> {uid(),dom()} | {adr(),dom()} | nokey.
pseudonym_pre( CurDom,Adr ) ->
		case Adr of
		{Uid,absent,CurDom} ->
			{Uid,CurDom};
		{_Uid,_Lab,CurDom} ->
			nokey;
		{_Uid,_Lab,_OtherDom} ->
			{Adr,CurDom};
		_ ->
			nokey
		end.

-spec pseudonym_post( dom(), {uid(),dom()} | uid() ) -> [adr()].
pseudonym_post( CurDom,Value ) ->
		case Value of
		{Uid,Dom} ->
			{Uid,absent,Dom};
		Uid ->
			{Uid,absent,CurDom}
		end.

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
			Values = ets:lookup( maps:get( pseudoTab,Db ),Key )
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

-spec alias_pre( dom(),adr() ) -> {uid(),dom()} | nokey.
alias_pre( CurDom,Adr ) ->
		case Adr of
		{ Uid,absent,CurDom } ->
			{Uid,CurDom};
		_ ->
			nokey
		end.

-spec alias_post( dom(),[ lab() | {lab(),dom()} ] ) -> [ adr() ].
alias_post( CurDom,Value ) ->
		case Value of
		{_Lab,_Dom} ->
			Value;
		Lab ->
			{Lab,CurDom}
		end.

-spec alias( db(),dom(),adr(),[ adr() ] ) -> [ adr() ].
alias( Db,CurDom,Adr,Accu ) ->
		PostProc = fun( Elem,ElemAccu ) ->
			[ alias_post( CurDom,Elem ) | ElemAccu ]
		end,
		Key = alias_pre( CurDom,Adr ),
		case Key of
		nokey ->
			Values = [];
		_ ->
			Values = ets:lookup( maps:get( aliasTab,Db ), Key )
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
%% defaults to CurDom (also in case of a remote).
%%

-spec flock_pre( dom(),adr() | {adr(),dom()} ) -> [ {idkind(),uid(),lab()} | {idkind(),uid(),lab(),dom()} ].
flock_pre( CurDom,Adr ) ->
		case Adr of
		{_Uid,_Lab,CurDom} ->
			Adr;
		{_Uid,_Lab,_OtherDom} ->
			{Adr,CurDom}
		end.

-spec flock_post( dom(), {idkind(),uid(),lab()} | {idkind(),uid(),lab(),dom()} ) -> adr().
flock_post( CurDom,Value ) ->
		case Value of
		{_Kind,Uid,Lab} ->
			{Uid,Lab,CurDom};
		{_Kind,Uid,Lab,Dom} ->
			{Uid,Lab,Dom}
		end.

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
			Values = ets:lookup( maps:get( flockTab,Db ), Key )
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
%%TODO: Split access into resources and comms?
%%

-spec impose( db(),dom(),adr(),adr() ) -> true | false.
impose( Db,CurDom,AuthnId,AuthzReqId ) ->
		lists:member( AuthzReqId,impose( Db,CurDom,AuthnId )).

-spec impose( db(),dom(),adr() ) -> [ adr() ].
impose( Db,CurDom,AuthnId ) ->
		Accu0 = [ AuthnId ],
		Accu1 = pseudonym( Db,CurDom,AuthnId,Accu0 ),
		AliasElem = fun( Adr,AliasAccu ) ->
			alias( Db,CurDom,Adr,AliasAccu )
		end,
		_Accu2 = lists:foldl( AliasElem,Accu1,Accu1 ).


