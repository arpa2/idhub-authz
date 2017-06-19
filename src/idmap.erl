%% Identity Mapper for ARPA2 IdentityHub Authorisation
%%
%% This process answers "As what may User A pose?"
%%
%% From: Rick van Rein <rick@openfortress.nl>


-module(idmap).

-export([
	init/0,
	idmap/1
	]).


-include("donai.hrl").


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


%% Key and val for the process dictionary
%% This involves sharing data from the inheritance tree
-type pd_key() :: fqn().
-type pd_val() :: [ { adr(),  pd_val() } ].


%
% Database management
%


%% Initialise by clearing the process dictionary
-spec init() -> none().
init () -> erase ().

%% Map an identity and flatten the result into a list
-spec idmap( fqn() ) -> [ adr() ].
idmap (authn_id) -> idmap_flatten( get(id) ).

%% Flatten a nested structure identity hierarchy
-spec idmap_flatten( pd_val() ) -> [ adr() ].
idmap_flatten( PdVal ) -> idmap_flatten( [], PdVal ).

%% Flatten a nested structure identity hierarchy
-spec idmap_flatten( [adr()], pd_val() ) -> [ adr() ].
idmap_flatten( [], Accu ) -> Accu;
idmap_flatten( [ {Adr,PdVal} | More ], Accu ) ->
			idmap_flatten(
				PdVal,
				idmap_flatten( More, [Adr|Accu] )).

%TODO% %% Insert an item in the process dictionary
%TODO% -spec pd_insert( pd_key() ) -> pd_val().
%TODO% pd_insert( Key, Val ) -> put( Key, Val ).

%TODO% %% Lookup an item from the process dictionary
%TODO% -spec pd_fetch( pd_key() ) -> [ adr() ].
%TODO% pd_fetch( Key ) -> idmap_flatten( get(Key) ).

