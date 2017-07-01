# Configuring Autuhorisation in ARPA2's IdentityHub

> *The authorisation component of the IdentityHub from the ARPA2 project
> involves clustering for high availability and load spreading, database
> sharding, and more useful things.*

On the InternetWide blog, we have described the
[identity considerations](http://internetwide.org/tag/identity.html)
leading to the
[identity model](http://idp.arpa2.net) for all ARPA2 projects.
Some aspects relate to authentication,
but the meat of the work ends up in the authorisation component of the
IdentityHub.  Details aside, the model consists of two types of data:

  * [identity inheritance](http://internetwide.org/blog/2016/12/18/id-6-inheritance.html)
    relations, what (alternate) authorisation user names are available to a
    user based on their authenticated identity
  * [access control](http://donai.arpa2.net/acl.html)
    lists, indicating the rights assigned to authorised users

We describe this
[in length](http://idp.arpa2.net)
elsewhere, but in essence we load this information into tables used by a
Diameter service that will cast `aye` or `nay` votes or, more precisely,
to an access level ranging from completely blocked-out to full empowerment.
Front ends use that information to decide what facilities are made available
to users: may the write or just read, can they create new content, and so on.

All this is background, but the topic of this document is operational.
So here we go.

## Clustering

The authorisation solution is programmed in Erlang, and inherits its
[facilitation for clustering](http://erlang.org/doc/reference_manual/distributed.html),
which roughly boils down to setting up a shared secret and telling
cluster nodes to connect to other cluster nodes.

We are going to assume that you setup clustering.  If you need to protect
the interconnect, you might consider IPsec, which is obliged in a
standards-compliant IPv6 stack.  It may turn out easier to setup than TLS
connections between Erlang nodes.

## Sharding

The data set of the tables mentioned may grow.  Internally, we use an
efficient in-memory representation of the data, where each table may hold
no more than 2 GB of data.  This can hold many millions of entries for each
of the relation kinds, but it certainly is not the sky as a limit.

The reason for accepting this aspect of the design is two-fold.  First, we
can rely on computers to use swapping to offload memory, should they need
to.  Second, not all data about everyone needs to be in the same set of
tables.

Internally, the authorisation model uses what is called *sharding*, where
data is partitioned over tables.  The smallest partition size currently
supported is a domain, but we also allow domains to be grouped into the
same database partition.  What we do is compute a fast hash over the
domain name being targeted, and partition data based on that &mdash;
for instance, we compute a CRC-32 checksum for the domain name and
take the outcome modulo 3 to split the data into 3 partitions.

Not all authorisation nodes need to carry the same partitions, as long as
the cluster as a whole does.  This is where ARPA2 authorisation becomes
scalable: just add machines to share the load, and partition the data
accordingly.

## High Availability

Authorisation is a vital part of pretty much all use of ARPA2 components,
so it is important to keep it up.  Making the service highly available is
easy to do, simply ensure that all partitions are always present on
multiple nodes; 2 is good, 3 is better.  Then, if a node goes down, the
others will learn about it and others happily pick up the authorisation
work for which they too hold the data.

In larger clusters, it is useful to spread the scatter the various
partitions as randomly or as evenly as possible.  Otherwise, when one
node of the cluster goes down, all the load might shift to one or just
a few other nodes, which can then get overloaded.  With a more balanced
spread of the work of the node that went down over other hosts, a more
even spread of the extra work load is achieved.

Consider an authorisation database split into 3 partitions numbered 0, 1
and 2.  A best split of their work over 3 machines would be

  * Machine **A** runs partitions **0** and **1**
  * Machine **B** runs partitions **0** and **2**
  * Machine **C** runs partitions **1** and **2**

Now, when **B** goes down, the work on partition **0** moves to **A** and the
work on **2** moves to **C**.  Assuming that they are of comparable size,
this should be an even spread and keep all the domains online.

Note that we use Diameter over SCTP, and when a node goes down it will
disconnect the SCTP carrier.  This should lead to a reconnect from the
Diameter client, to one of the supporting hosts.  Diameter messages are
internally forwarded to the proper node, but even the client would be in
a position to respond to downtime on **B**.  A different but similar story
applies when running Diameter over AMQP.

## Load Balancing

The ability to replicate partitions can be very helpful in offloading a
machine when it gets too busy to handle requests with a good response
time.  The scheme presented for high availability already does some of
this, by spreading 3 partitions over 3 machines instead of having just
a single machine for one partition.

It is possible however, to setup more playful combinations like:

  * Machine **A** runs partitions **0**, **1**, **2**
  * Machine **B** runs parittions **0**, **1**
  * Machine **C** runs partitions **0**, **2**
  * Machine **D** runs partitions **1**

This is not evenly balanced, but it may make sense if machine **A** has more
cores than **B** and **C** and when **D** is only a single-core machine, say.
When splitting into more than 3 partitions, there is an option of more and
more fine-grained control.

What is useful to understand is that machines can load any set of partitions
of the database that one likes.  The cluster communicates internally what is
available on which nodes, and routes messages accordingly.  The configuration
of a node can therefore also change the partitions it handles, and the
cluster will adapt.  In fact, when data is not available, it may be obtained
from cluster nodes that already have it (if the data is stable and of the
desired version).  There is also a mechanism for bootstrapping the data from
a backend store in LDAP, of course, and this mechanism is also used to adapt
to changes to the IdentityHub configuration for the various domains.

It is useful to understand that, although nodes are configured independently,
that it is the use of the same fast hashing algorithm and the same modulo
that makes this flexible interaction possible.  When mixing algorithms, there
is a greater chance of having to bootstrap from LDAP, which is time-consuming
in the phase of bringing up the service.

The flexibility is very useful in recovery from machine failures.  One can
simply erect a new machine, install the authorisation node, connect it to the
cluster.  Then, by adding partitions of the authorisation database to the
new node, it will start pulling them in from other nodes and be ready for
rock 'n' roll in the blink of an eye.  A reason we can rely on in-memory
data is that replicas are usually up in the air, somewhere nearby.

## Example Configuration File

The configuration file is an Erlang term.  It lists the quick hash algorithm,
the modulo number or equivalently a number of bits, and which partitions of
the data are present on the node.  We might choose different module/prefix
settings for some things, for instance to accommodate a high-profile domain
for a valuable customer.

```
#{
	hash => crc32,
	modulo => 3,
	partitions => [0, 1]
}.
```

This information is distributed between nodes of the cluster, so it is
strictly local.  Information is spread as soon as the corresponding data
is ready for service.  Information will be indexed by the node name, which
is part of Erlang's clustering setup.

Imagine the result of this configuration being that partition data is loaded
for domains whose CRC-32 hash, taken modulo 3, ends up being 0 or 1.  So,
the value 2 is dropped, thus reducing the database to 2/3 of its size.

The general idea is that domain data sized `D` can be spread over `C` cluster
nodes with `N` replicating nodes when each nodes stores on average
`D * N / C` as their data size.  The simple format shown above helps to control
that.

## Tool Support

We are interested in seeing tooling around the authorisation component,
involving the following things:

  * Investigate lacking number of replicas for database partitions
  * Perhaps reconfigure the cluster by shifting partitions between machines
  * Distribute high-demand domains over more nodes automatically
  * Monitoring machine load, in terms of response time
  * Providing metrics for cost calculations

To facilitate such tooling, we intend to publish information over SNMP:

  * Recent average response times for nodes handling requests
  * Partitioning information, including partitions available on each node
  * Requests handled per partition, and perhaps even per domain
  * Number of identity/acl comparisons made per domain, and per query

