%% Identity Mapper for ARPA2 IdentityHub Authorisation
%%
%% This process answers "As what may User A pose?"
%%
%% From: Rick van Rein <rick@openfortress.nl>


%% The database is a collection of ETS tables that will be updated to
%% accommodate the idmap and xsmap setup.  There may be updates to the
%% database in order to commit changes.
-type db() :: #{}.


%% The 'idmap' database is a memory-stored cache that unfolds
%% the inheritance of ARPA2 identities.  The mapping is keyed
%% by an externally authenticated identity and lists the
%% corresponding authorised identities.
%%
%% The storage tables can be shared between multiple processes.
%% This is a standard property of ets, and it helps to achieve
%% concurrent workers who can efficiently pass table data.
%%
%% When starting up, the entire structure must be built from
%% scratch.  This would be awful, if it wasn't for the cluster
%% that allows nodes to go offline individually.  Spread your
%% cluster geographically and you should not have incredible
%% downtime, and when power surges do occur the rebuild is not
%% likely to be the slowest part of the offline period.
%%
%% When we add cluster communication to notify availability
%% of domain groups on nodes, we may want to retract service
%% before registering, and only resurrect domain service after
%% having set them up.  Until a domain is resurrected, it will
%% be handled by other nodes in the cluster.
%%
%% This design is optimised for incremental updates; as arrows
%% are added and withdrawn from the inheritance diagram we
%% should modify the data.

-type idkind() :: user | pseudonym | alias |
                      member | group | occupant | role.

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


