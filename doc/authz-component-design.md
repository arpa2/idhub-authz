# Designing ARPA2 Authorisation

> *This article explains the design of the ARPA2
> component for Authorisation in the Identity Hub.
> It implements our behavioural considerations and
> offers a truely beneficial component on which the
> rest of the IdentityHub infrastructure can build.*

There is a vast list of requirements for the Authorisation component in the IdentityHub:

  * It should be highly available
  * It should introduce minimal delay times
  * It should answer various questions

We have made a number of design choices to accommodate these requirements.

## Use Diameter over SCTP

The Authorisation service's main protocol is Diameter, in part because it is the more capable version of industry-strength RADIUS.  But there is a much bigger reason, and that is its reliance on SCTP.

Where TCP offers reliable delivery, and UDP avoids head-of-line blockages, SCTP combines these properties when frames are sent unordered.  This is indeed the mode of operation prescribed for Diameter's use of SCTP.

The result of choosing SCTP instead of UDP means that we need not assume repeated submissions of a question by an impatient (and uncertain) client.  SCTP is rock-solid in its assurances that a frame is being delivered, up to the point of termination of the connection.  This greatly simplifies the service, which now has no more need for caching requests that are being processed.

Compared to TCP, a loss of a single frame does not constitute a delay for all other frames sent after it.  The SCTP framework permits the Diameter server to continue to work while resubmission of the missing frame is being requested.

## Programming in Erlang

The Erlang language is a close fit for the purposes of the Authorisation system.  Even though it is function, does garbage collection and runs on a virtual machine, it is known for its high efficiency, stability and fault tolerance.  It regularly outruns C and C++ in a massive way on these kinds of services.

Erlang supports Diameter over SCTP in its functional library, and interfaces the requests to the specific lingo of Erlang.  This provides quite a bit of vital code from a consolidated basis.

## Additional AMQP Service

The components for the IdentityHub are connected over AMQP, or at least that is one of the modes of operation.  It is likely that the ServiceHub phase will use the same technology.

AMQP is a powerful choice for component connectivity because it can be manipulated independently of the applications themselves.  In that sense, it functions like a bus for messaging purposes.

We have selected the Apache Qpid project as our AMQP backbone, and that means that we choose AMQP 1.0 instead of the older 0-9-1 or 0-10 versions that relied heavily on the concept of a queue.  AMQP 1.0 can work without queues, and is a symmetric protocol.  It is an Oasis standard.

Erlang does not appear to have support for AMQP 1.0 yet; the closest approximation is RabbitMQ with the 1.0 plugin, which is a bit more overhead than worthy.  Ideally, there would be a `gen_amqp` transport that linked messagging into Erlang in the usually manner.

Lacking this, it seems prudent to build a relay, and that is in fact a very useful component to have:

 1. Connect to AMQP using Qpid Proton for C++
 2. Retrieve messages, and forward them to SCTP
 3. Support SCTP fallback, with `localhost` as primary
 4. Acknowledge messages once SCTP acknowledges them
 5. Upon SCTP teardown, requeue unacknowledged messages
 6. Recognise component overrides, and relay those over AMQP

All this is generic; we may also have to add protocol specifics:

 1. Recognise responses and pass those to AMQP `Reply-To` addresses
 2. Possibly add or replace identifying information for responses
 3. Possibly use information from the query (such as the targeted authorisation identity) to distribute queries, so as to optimally use node-local caches.  Note that use of SCTP by applications also implies caching benefits, but without the ability to redistribute in middleware.

Clearly, our AMQP-to-SCTP forwarding solution will have a generic part and an application-specific part.  In fact, it may also have to incorporate other carriers, such as TCP, to accommodate even more protocols.  Yes, this is getting awfully close to our Funnel Tunnel design, but that is no surprise.

For Diameter, we shall introduce attributes holding the `reply-to` address and `correlation-id` to be used with AMQP; this means that any AMQP-to-SCTP node can be used to send a response message.

## Dealing with Transitive Closure

The complex interactions between the various forms of identity, and specifically the loopback for groups, means that the general problem of dealing with identity inheritance comes down to solving the problem of transitive closure.

Fortunately, the instances of the identity inheritance diagram is relatively simple &mdash; it is always a tree.  When `A` inherits from `B`, which in turn inherits from `C`, then `A` indirectly inherits from `C` as well.  In fact, it may also inherit directly, and/or through other paths.

In general, we should count the number of such paths between each pair of identities.  When `B` inherits from `C` over 2 paths, then `A` inherits from `C` over 2 paths times the number of paths from `A` to `B`, plus any other paths that do not pass through `B`.

Note that `A` will list `B` as well as `C` in its transitive closure.  So, when removing the direct link from `A` to `B`, we can subtract from the counters for anything pointed at from `B`, including `C`.  In fact, the values deducted will be multiplied by the number of paths over which `A` inherits from `B`, which is something it tracks too.  Adding a link does the same thing in the opposite direction, of course.

Since all changes will be such additions and removals of individual links, we can incrementally update the database accordingly.

All we need now is an initialisation strategy, which can be established by working through the tree in a bottom-up fashion.  Later additions may refer back to already-defined values.

The one thing we need to be cautious about, is that cyclic inclusions could be problematic (even though a trivial definition of its meaning is possible).  For now, this leads us to forbid groups as members of groups, the only path over which such mishaps are possible.  Note clearly how this mechanism allows us to dig up relations for a given authenticated identity alone.

## Caching Strategy

Authorisation nodes have *local* caches holding information learnt through expensive queries involving multiple-table joins.  In fact, the cache holds two kinds of mappings:

 1. Whether an authenticated identity can pose as a different authorisation identity
 2. Whether an authorisation identity can access a particular resource

### Identity Mapping Cache

The first cache stores information such as group membership and role occupancy.  It will even involve a transitive closure on group membership, once we allow that to be recursive.

This cache holds both positive and negative results; it will learn the negative results quickly, though a delay may be of interest in light of security.  Positive results are helpful in providing confirmations more quickly, so in optimising a user's experience of responsive authhorisation.  Entries not found in the cache incur a full computation, which may be done on demand, or as soon as configurations change.  Cache entries will be removed when configuration changes occur, so as to cause their recalculation.

Cached negative results are helpful in avoiding repeated computations that are destined to fail.  This will not fill up with things that can easily be checked to be invalid; non-existing user names or labels will not end up being cached, but are instead checked explicitly and quickly at the start of any full query.

It is possible that we would add Bloom filters to further speedup authorisation caching, but these would probably have to be a combination of positive and negative filtering.

The table has the structure **(TODO:SQL?)**

```
create table idmap_cache (
   domain varchar(255),
   authn_user varchar(255),
   authz_user varchar(255),
   path_count unsigned integer
)
```

This does not have a unique key, but is looked up from `(domain,authn_user)` to `(authz_user,path_count)`, so **(TODO:SQL?)**

```
create index idmap_index
on idmap_cache (domain,authn_user)
```

### Resource Access Cache

The second cache involves *compiled* versions of Access Control Lists.  More accurately, the entries in white lists and black lists are mapped to SQL rows, so they can be used in a query anything that may be set for the resource.  Any use of generic ACL patterns or other reuse frivolities have been flattened out of this form.

The table has the structure **(TODO:SQL?)**

```
create table xsctl_cache (
   domain varchar(255),
   uuid_resource uuid,
   authz_user varchar(255),
   xsctl_ok bool
)
```

This does not have a unique key, but is looked up from `(domain,uuid_resource)` to `(authz_user,acl_ok)`, so **(TODO:SQL?)**

```
create index xsctl_index
on xsctl_cache (domain,authz_user)
```

For now, we assume that the `authz_user` can be used to construct the suggested user identity to be perceived by this resource.


### Combining the Caches

The two forms of cache represent orthogonal aspects of authorisation; and indeed, there are independent formulations in Diameter to get to them.  To combine them, it is imperative that they are each represented as a single table, because this makes the non-cached combinations relatively inexpensive: **(TODO:SQL?)**

```
select idm.path_count > 0 idmap_ok,
       acl.xsctl_ok       xsctl_ok
from idmap_cache idm,
     xsctl_cache acl
where idm.domain = ?domain
and   idm.authn_user = ?authn_user
and   acl.domain = ?domain
and   acl.resource = ?uuid_resource
and   (
          idm.authz = acl.auhtz_user
       or ...)
```



Note that more elaborate forms of the last line are probably needed; but at the point we have come here, that should not be an incredible problem anymore.  The whole purpose of this order of querying (and the assumption that a query processor would leave that in tact) is that the search is started from the two ends represented by the tables, and only a few tests still need to be made.  We may be able to code this in SQL, or perhaps in an imperative-language plugin for the underlying database.

Results from authorisation are considered good for the duration of a session, whatever that might be.  Independent mechanisms may be used to permit instant processing for long-lived sessions; in lieau of that, long-lived sessions may also req-request authorisation on a regular basis.

The query shown above yields multiple answers, and a selection must be made based on black/white list logic and the possible absense of cached information.

## Local Caching

The authorisation component is intended to support distribution.  The reasons for this may be both high availability and performance.

Although Erlang provides an excellent infrastructure for distribution of the cache, we shall not use this.  Authorisation is a mostly read-only process, so we can run completely independent processes.  The one thing we might do however, is forward authorisation requests to a better equipped node, if one is available.

This means that we have a simple mechanism for distribution of inquiries, and a good cause for local caching.  Caches with local information can be smaller, and so be looked into more quickly, than ones that hold all the information in all the nodes.  Since they are caches, they can be rebuilt as desired.

It may in fact be prudent to distribute ACL information to other nodes than identity mapping, and keep both local.  This means less data replication, but more communication between nodes.  Basically,

  * resources have a *best node* in which their information is primarily cached
  * even simple SCTP/Diameter clients can connect to the best nodes, because they usually represent one particular resource
  * authenticated users have a *home node* in which their mapping to authorised users is primarily cached
  * a simple protocol between a *best node* and a *home node* can establish a direct link
  * an alternative strategy could be to use a domain name as a distribution key, because that means the *best node* and *home node* are usually the same

Note how well this pattern of communication matches with Erlang.  In fact, the cache design does not even have to be an SQL database anymore; it might work through a key-value setup for the two sides of the query.  In such a setup, we might find a *bag* of entries from each side, and end up matching those one by one.

Upon receiving a request to authorise the entire path from an authentication identity to resource access, requests may be dismissed to their respective localised handler tasks.  When handlers are remote, a request is relayed to them; upon failure, a decision is made to handle the request locally.  When a node has been offline before but no longer, the locally cached backup data may be removed.  A general cache cleanup mechanism might take care of this when data has not been used for a while.

Nodes to which work is delegated will need to know where to send their response.  For direct SCTP use, this will be the node that handles the SCTP connection.  For AMQP clients however, it is possible to process an attached `reply-to` address and `correlation-id` to send over AMQP from any of the nodes that have an AMQP-to-SCTP frontend.  (But this may mean that the AMQP-to-SCTP frontend cannot know for sure if the message will be responded to, and whether it may be acknowledged already.)

## Redundant Caching

The caching mechanism may be local, but it can still be replicated over multiple nodes.  The saved effort can still be large when the number of nodes exceeds the number of replicas.  This is how a redundant cluster of nodes can scale without ever-growing database sizes as a limiting factor.  Effectively, the data stored is about the number of entries over the entire system, times the number of replicas, divided by the number of cluster nodes.

Cluster nodes may have multiple workers, and each shall have a separate name.  Some machines may have 4 workers, while others have 16 &mdash; it all depends on the level of concurrency (the number of CPUs) and storage size made available.  The workers on a machine can substitute for each other and one is randomly selected under the assumption that this will balance the work load evenly.

**TODO:WORKERS?** *It may actually make sense to assign a particular local database to each of the workers, instead of making them equivalent.  We don't assume that they are and could therefore list the complete names of workers@machines for each group of domains.  The advantage of local databases would apply to this local situation as well.*

Replicas tend to run on different machines, and are distributed manually.  This means that operational staff can take things such as geographic location into account, as well as terms of service and other non-technical issues.  At some point, we will probably start to support moves of replicas between machines.

The replica machines are located from the domain name.  This is hashed (with a fast insecure hash) to find a scattering code.  The code is divided over the number of replicas, and then used to find a node from the list setup for the domain.  Within the selected machine, a further division is used to determine the worker to address.  **TODO:OR_NOT?**

Of course, when communication fails, a fallback machine is selected in the hope that this will be online.  There will be no attempts to contact another worker on the machine that failed, as it is more likely that a complete machine will fail than just a single worker on it.

**TODO:REMOVE_FALLBACK_PROCESSES**

## Redesigning Caches in Erlang

We specified SQL code above; below, we map to native Erlang database storage instead.

### Identity Map Cache Entries

An identity mapping provides a mapping from an authenticated identity under a domain, to an authorisation identity.  The authenticated identity may or may not be local to an IdentityHub's managed domains, so the full form `user@domain` is used in *some* cases.  In general we will need to look under a domain, so this notation turns out to be practical in *all* cases.

 1. The IdMap Cache maps a `user@domain` style string to a bag of authorised identities.
 2. Each of the authorised identities take the form `{userid(),label(),domain(),pathcount()}` where the `domain()` may be set to the atom `current` to optimise storage; in this case, the part after the last `@` in the key will be assumed.
 3. We always know the targeted domain; it is either the resource's domain or the domain of the requested authorisation identity.  Anything mentioning another domain can be stripped from the bag before it is sent to the combining agent **(TODO:FOREIGNERS?)**.  In short, we derive the bag elements that have `current` as their domain, and relay only those.

**TODO:CLEANUP?**

### ACL Cache Entries

An ACL is either compiled to cache or not.  Among the compiled forms is one that has neither black nor white lists, to accommodate the absense of any ACL settings.

 0. The ACL Cache maps a `{resource(),domain()}` pair to the compiled ACL form; absense of a key triggers looking it up
 1. The Erlang form of a compiled ACL is a pair `{whitelist(),blacklist()}`; the atom form `absent` is used to cache confirmed non-existence of an ACL for a given key
 2. Both the white and black lists are lists of ACL entry lines `[acl_entry_line()]` and the empty list `[]` is a form to explicitly state the absense of either
 3. An ACL entry line holds an identity pattern, split into its components `{userid(),label(),domain()}` where each of the components can be set to the atom `absent` if it is not present in the ACL form, or `anything` to indicate a wildcard.  Note that the form `john+@domain` differs from `john@domain` by using the atom `anything` as a label in the first form, and the atom `absent` for the second.  When a domain is `current`, this is a storage/retrieval optimisation, and will be substituted with the targeted domain; here, it is worth noting that `@.` contains a non-empty domain, but the form is written as `anything` because trailing dots will always be stripped and then the initial dot that indicates a pattern is missing.

**TODO:CLEANUP?**

### Combining the Cached Data

The two forms come together in the agent that formulates the response to the authorisation inquiry.  This assumes the complete authorisation request, from an authenticated identity to a resource.

The combination takes `current` into account as a possible, and matching, domain on both ends.  It considers the ACL entries to form patterns, for instance a domain starting with a dot is matched with the ending of the authorised identity's domain instead of with the whole domain, and `anything` is also handled.

```
% match_domain/3 checks if a compiled ACL line matches
%  arg_1 is the targeted domain name from the resource
%  arg_2 is a compiled domain from a compiled ACL line
%  arg_3 is a domain name from an authorised identity

match_domain Dom ['.'|Rest] Inst  ->
	case Inst of
		current -> CmpInst = Dom,
		CmpInst -> true
	end,
	lists:suffix (['.'|Rest], CmpInst)

match_domain _ anything _ -> true
% match_domain _ current current -> see _ Patn Patn
match_domain _ Patn Patn -> true
% match_domain _ _ current -> false -> see _ _ _
% match_domain _ current _ -> false -> see _ _ _
match_domain _ _ _ -> false
```

## Authorisation Processes

What we are now ending up with, is a few (database) processes:

  * An *identity_mapping* process, looking in the local cache database
  * An *access_control* process, looking in the local cache database
  * An *identity_mapping_backup* process, acting like the identity mapping but (in-memory and) as a temporary substitute for the *home node* **TODO:REMOTE_BACKUPS**
  * An *access_control_backup* process, acting like the access control but (in-memory and) as a temporary substitute for the *best node* **TODO:REMOTE_BACKUPS**
  * An *identity_bagger* process, providing requested updates to the identity mapping or its backup by computing a transitive closure from an authenticated identity
  * An *access_bagger* process, providing requested updates to the access control or its backup by compiling an access control list
  * A *response_composer* process, joining the identity mapping and access control to form a response
  * A *query_agent* process, which orchestrates request handling over the other components

### Dealing with Crashing Processes

Our code will be bug-free of course `;-)` but just to capture spurious hardware glitches...

The *query_agent* will overview the other processes, and see if they exit unintentionally.  When this happens, the corresponding SCTP connection(s) will be torn down to signal to the client that no more responses are to be expected.  The client should then reconnect and ask again.

This is the most brutal approach, avalanching internal traces of trouble.  A more refined alternative could be a local cache for unanswered queries, and permitting those to be reformulated.  This need not even be complex; the *response_composer* may match these messages when they arrive in its mailbox.  After a crash of one or more components, a `reset` message from the *query_agent* is all that is needed to send the original messages back for renewed processing.

Of course, when a large portion of the program crashes, the same approach is still of use.