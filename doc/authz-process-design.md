IdentityHub Authorisation: Process Design
=========================================

>   *Erlang offers great flexibility in the way that we can split the
>   functionality of authorisation over processes. This is how we do it.*

Hash algorithms
---------------

Domain names are grouped together on the basis of a common hash. One hash can be
helpful to derive a small number that serves as a basis for locating the serving
nodes for that domain.

We can imagine a few hash algorithms over domain names:

-   `literal` treats the domain name itself as though it were a hash value, so
    it can be looked up literally. This weighs on the higher-up processing
    linearly to the number of such literals, so limit its use to a few high-end
    domains.

-   `atom` is a variation on the latter, where the keys are not strings but
    Erlang atoms, but the same caution applies.

-   `{crc32mod,N}` computes the CRC-32 checksum of a domain name and then its
    remainder modulo `N` to get a good spread of the domain. This is a good
    workhorse for the masses. Note that the parameter `N` can vary, so we might
    have a few variations. This adds no value, except during reconfiguration of
    the cluster.

-   `{crc32mask,M}` computes the CRC-32 checksum of a domain name and then its
    bitwise masking with `M` to get a good spread of the domain. This is a good
    workhorse for the masses. Note that the parameter `M` may take any
    cross-section of bits, so we might have overlapping or orthogonal variations
    (which is also doable with `{crc32mod,N}` but with a little less control
    over sizes of the various cross-sections. This is a more complex variation
    during reconfiguration of a cluster. *Not implemented yet.*

A hash value is the combination of a hash algorithm and its outcome. For the
aforementioned algorithms, a few examples of hash values are:

-   `{literal,<<"internetwide.org">>}` (or a long integer form?)

-   `{atom,'internetwide.org'}`

-   `{{crc32mod,5},3}`

-   `{{crc32mask,255},183}`

The third form is very useful, because we can specify in a node configuration
that it will hold hash values 3 and 4, while others take on their own outcomes.
For now, such configuration is done manually.

The default hash algorithm, in lieu of any configuration, is `{crc32mod,1}`
and the default hash value that is added when none is configured, is
`{{crc32mod,1},0}`.  This means that a default node holds all domains with
the very small overhead that enables it to smoothly transition into a
cluster-based solution with a different modulus but otherwise matching
hash algorithm `crc32mod`.  This would not have been possible had the default
hash algorithm been `literal` or `atom`.

**TODO:** How do we get to the configured list of hash algorithms to use? We can
parse it from the routing table, but that would not assign their priorities.
Also, it would be expensive to do that for every iteration!

Routing Table
-------------

Every node maintains a routing table maintenance process, called `routab`. The
routing table is used as a root to start from, and is a registered ETS table
name `routing_table`. The nodes' `routab` processes communicate about changes to
tables, so they retain the same copies. Specifically, they inform each other
when entries are added or removed.

The routing table maps hash values such as `{{crc32mod,5},3}` to a triple
holding

1.  One locally active fragment process or `undefined`; such a node is always
    tried first because of efficiency;

2.  A list of remote active fragment nodes; these are tried in a seemingly
    random order until one responds;

3.  One process or `undefined` that serves as a database backup; it is set as
    the heir of data tables in the local fragment node and may be used when
    restarting such a node. This saves a crashing fragment process from having
    to retrieve all data using more expensive procedures.

The processes enlisted (or entupled) are mentioned by their Pid, which serves
for prodding at them directly.

When freshly started, or restarted after a crash, the `routab` process is empty
and it has no database left. It does have a heir for the routing table however,
and it will deliberate with that heir to take ownership yet again. When the heir
was not found, then the `routab` process will ask its peers for their data. In
addition, it is intended that fragment processes detect the demise of the
foregoing `routab` process, and register once more with the new instance.

**TODO:** There is no support yet for reconfiguring the fragments, effectively
needed to alter cluster configuration.

**TODO:** Local cluster configuration with `{{crc32mod,5},3}` may be derived
from a cluster-spanning table (possibly the routing table) that lists nodes for
each hash value.

Fragments: Database Management Processes
----------------------------------------

Every fragment represents a fragment of the space of domain names. Usually, this
represents a hash value such as `{{crc32mod,5},3}`. The data stored for this
fragment can be limited in size, which is why the modulo to be used is a
configurable number. The more fragmentation of the table, the smaller the
individual tables are. There is no use, other than keeping memory consumption
within bounds, to set a very high number here.

Fragment processes register with the `routab` process as soon as they have been
filled with data. The `routab` process links or monitors the process before
entry, so it also knows when to remove the fragment. When the process is new, it
distributes the process to `routab` processes on other nodes as well. Removal is
rumoured using the same mechanism, though there are also self-taught methods
that involve a `ping` / `pang` / `pong` exchange. Note that the registration
mechanism is different for local and remote registration, which ends up setting
different elements in the `routab` value.

A `routab` process is started without knowledge of domains; it is simply handed
the hash value to serve. It then looks in the routing table, hoping to find a
heir from a predecessor. When lucky, it will ask for the data and retrieve the
tables and take ownership. This procedure may however fail if the backup does
not currently own (all) the tables desired. After having answered a few queries
successfully, the fragment process will once again assign the lower process as
their heir. (**TODO:** Immediately for now, but that is not the most fail-safe
option as the tables might be causes of crashing.)

Note that it may be necessary during cluster reconfiguration to rely on a number
of cluster fragments already in the `routab` database. For instance, going from
`{crc32mod,3}` to `{crc32mod,5}` may involve querying a number of the old
fragments for several of their `{crc32mod,15}` values, where 15 is the smallest
common multiplier for the moduli 3 and 5. To make this work, a fragment would
need to setup a "plan" that is complete, and then it would see it to completion.
Note that this also means that queries for dumps between fragment processes can
be limited to a partial range.

When not successful at provisioning itself over a predecessor's heir, the second
thing to try is to look on the network for others replicating the data. This is
done by looking in the `routab` for other processes serving the same hash value,
and asking them for a database dump. Such dumps are in the interest of the
serving process, because they help to spread the load of resolving queries.
Serving the tables is passed over to a secondary process, so resolving can
continue. Again, once successfully completed, the fragment happily enters
service by registering with its local `routab` process.

As a last resort, a freshly started fragment process can query SteamWorks
Pulley, and have a full dump made towards it from an LDAP configuration. This is
considered a relatively expensive operation, though we may at some point find
that it is less of a burden on other nodes than transferring all the data. There
is some elegance in a network being able to keep its own data on board.
**TODO:** For now, rely on SteamWorks only.

When all this fails, a massive error needs to be reported, as something is
really wrong. But normally, one of these three procedures should succeed in
feeding the fragment process with its databases and it can register with
`routab`.

So now we turn to the serivce provided. There is a `ping` to `pong` translation
in the interest of liveness checks, but the bulk will be domain database
queries.

Domain database queries are looking to retrieve a set of databases, without
posing a query to either of these databases yet, for local use in an
authorisation process. These individual authorisation processes are the ones
posing the question, as they overview individual query processing and error
handling. These queries ask for `{dbget,Pid}` and they return `{dbgot,DbMap}`
where the `DbMap` holds the usual map with `aliasTab` and so on.

Finally, these processes are open to dynamic updates. The messages for this are
`{add,TODO}` and `{del,TODO}`. Such messages may be initiated from a SteamWorks
Pulley backend, which subscribes to configuration changes in LDAP so these
directly translate into behavioural changes in the authorisation service. There
is a transactional side to these as well, which is achieved by replacing table
entries with a map with keys `txn_live`, `txn_changed` and `txn_prepare` keys,
for what is being served, what changes have been offered, and what changes are
prepared for commit. During a prepare-to-commit, the entries in `changed` move
to `prepare`; during `commit`, the entries in `prepare` replace what is
`txn_live` and during `rollback` any entries in `txn_changed` or `txn_prepare`
are removed. **TODO:** More detail, such a transaction identifiers, may be
needed in the `prepare` version, so recovery can lookup what has been changed in
the past. For now, the assumption is that restarts of a fragment imply
continuation of a transaction, where a failed `prepare` is likely to have been
reported back to SteamWorks Pulley.

Once registered with the `routab` process, fragment processes also monitor this
`routab` process. When it dies, the fragment process will start polling until a
new and empty `routab` process has been started. This empty process will be
provisioned by registering the current process once more.

Authorisation Handlers
----------------------

Every authorisation handling process is a local lookup of the fragment
databases, on which `idmap` and `xsmap` processes are run as desired.
Authorisation handlers are run locally, though it will be regularly done to
fulfil remote inquiries.

Note that the relative frivolity of dealing with transactional elements in the
`idmap` and `xsmap` databases ends up in the various mappings. This cannot be
avoided, but its effects are as simple as mapping the current data, as in

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mymap #{ txn_live := TxnLive } X Y Z ->
        mymap TxnLive X Y Z;
mymap {normal_case,...} ... -> ...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Such elements are added to the `idmap` and `xsmap` querying procedures. Note
that this makes change processing atomic, but not isolated from use of the data.
Had we wanted that, then we might have added a map from database version to
values (and landed ourselves with a problem of when to expire data that is being
replaced). For the authorisation case, there does not appear to be a cause for
isolation, just atomicity will do (and even that is questionable, but in light
of other transaction aspects that might be involved it may help to avoid
glitches during times of failure).

Client Agents (Diameter, HTTP, ...)
-----------------------------------

Finally we turn to client agents, which are request-specific processes started
by Diameter, HTTP servers, and so on.

Client agents figure out what authorisation question to ask (imposing identity
and/or communication or resource access) and then start a general process
consisting of querying the routing table for fragment processes to ask. Based on
the response, these fragments are tried; first locally, and then, in arbitrary
order, all of the remotes; this process is meant to iterate over alternatives,
ending when one provides an answer, whether positive or negative. The next
element in line is only used when monitoring reports a failere or, perhaps some
day, when a query takes too long.

For each of the fragment processes, we derive the node on which it runs, and
spawn an authorisation handler process on the same node, forwarding our inquiry
to it. The authorisation handler finds its local database from the fragment Pid
that we send it, and sets out querying the databases. It responds to us,
possibly records some statistics (such as cost and running time) on its node,
and terminates normally. Monitoring of the remote process is used to take note
of unexpected failure, which will lead to the fallback scenario based on routing
table alternatives.

Database Heirs
--------------

A heir is a very solid, predictable process. All it does is receive values, and
report them back. The values pass in and out as
`{'ETS-TRANSFER',Tab,FromPid,DbMapKey}`. Finally, there is a query named
`{hand_over,DbMapKey}` to request from a heir that it hands over its inheritance
databases to the database handler process.

The same heir code can be used for the `routab` and fragments processes.

Supervision Infrastructure
--------------------------

This is not complete yet.

Fragments and the `routab` process are independent, and they will learn from
each other, so they reside under a `one_for_one` supervisor, or may live
entirely independently.

Fragments pair up with a heir, but they are meant to be run independently, so
again, `one_for_one`. The same applies to the `routab` and its heir.

The application has under it, directly or indirectly:

-   the `routab` process and its heir

-   the fragments as configured, and their heirs

-   the client agents such as Diameter and HTTP

The instance processes for client agents and authorisation handlers should be
monitored or lined by their initiators, so they should be fine. They also
monitor the infrastructure that they talk to, so they should never need to get
stuck.
