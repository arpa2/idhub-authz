# Authorisation Cluster and Modulo Migration

> *Clusters usually work by keys, which they take modulo-number to decide
> with a simple match what is local and what is not.  Now, what would be
> cool is when this were changeable.  And with functional programming,
> that is actually achievable.  An Erlang success story.*

We will specify for each node that they will process, say

  * `crc32(domain) % 3 == 0`
  * `crc32(domain) % 3 == 2`

This keeps 2 out of 3 portions on the local node.  It will be stored in
two different
[ETS databases](http://erlang.org/doc/man/ets.html)
where each table may grow up to the size limit of 2 GB that applies to ETS.

## Changing the Database Split

We may at some point want to change our cluster and, most importantly,
the modulo factor and/or what parts are available locally, because:

  * We may need split up ETS tables as they approach 2 GB each
  * After node breakdown, we may need to redistribute the load
  * After node recovery, we may be able to offload other nodes
  * For scaling purposes, we may have extended the cluster

In all those cases, we might temporarily add extra definitions, as in

  * `crc32(domain) % 3 == 0`
  * `crc32(domain) % 3 == 2`
  * `crc32(domain) % 5 == 0`
  * `crc32(domain) % 5 == 1`

The cluster now starts to re-align its databases and the corresponding
router tables.  Then, after it has all been committed, we can reduce to

  * `crc32(domain) % 5 == 0`
  * `crc32(domain) % 5 == 1`

We use the intermediate phase to be sure that data does not drop out of
the cluster, as that could lead to fresh buildup from LDAP, which is
tedious.

## Avoiding Overlap with Least Common Multiple

There is a concern that the intermediate period may increase the load on
memory.  While it would be equally possible to setup temporary machines
and offload the old ones while installing the new ones, this is not
always practical.

As it turns out, ETS tables do always copy the data that is ladled into
them, even when they originate from other ETS tables in the same process.
The concern for memory usage spikes is therefore warranted.  We may
expect to use swap more intensively during the transition, but that is
hardly a burden *if* data is always delivered from the same table when
multiple are available.  This is easily arranged with an ordered
routing table.

What will be done when some domains are modulo-3 and others modula-5,
is that the
[Least Common Multiple](https://www.mathsisfun.com/least-common-multiple.html)
of these modulo-values is used, so 15 in this case.
So, we take the routing table that used to be modulo-3 based, and
turn it into a modulo-15 routing table.  More entries, and a finer
potential spread than before.

The routing table targets processes when the M-modulo-N matches.
The extra routing table entries simply share a process identifier
which then is capable of finding the domain information in a
table whose modulo is actually a divider of N; this is the result
of using a (least) common multiplier in the routing table.

This same generalisation also applies to the new situation, with
the same M-modulo-N but with backend processes that works modulo the new
factor, but also a dividor of N.  While both modulos are being used,
the routing table could point to the old or new process, or both,
it would not really matter.

During this period where either process can serve the overlapping
portions, the data can be pumped from old to new, by asking for
the M-modulo-N portions through the domain request router.  The
router will follow the same rules as for Diameter requests to find
a provider of data, and it will prefer local nodes.  This means that
the new information is taken from a local node when possible, or from
another when it must.  The same failure fallback arrangements that
are used for Diameter processing is also used now.

At some point, the older entries (modulo 3 in the example above)
are removed and the configuration is processed once more.  The
routing table now *wants* to migrate from being modulo-15 to
being modulo-5, thus forgetting the old modulo-3 situation,
but data must be complete in the new processes to enable that.
To ensure this, the routing table is recombined and any missing
data is retrieved from other nodes or, as a last resort, from LDAP.
When a new process has taken over a least-common-multiplier set of
domains, the older process can be cleaned up, and its memory
recycled.

This mechanism enables online transition from one modulo system
to another, with maximum use of local information and minimal
communcation between nodes.  With the procedure sketched above,
it is possible to migrate a live system to a new configuration
without any downtime.

## Analysis and Warning System

It may be a good idea to verify the coverage of a cluster, and
especially during changes such as described above.  Being able
to detect when the number of replicas drops from 3 to 2 or even
1 is really helpful in establishing high availability.

This information may be used as signalling to operational staff,
but it can equally well be used to retain an old table somewhat
longer, namely until other nodes have announced that they have
completed a migration of data to their node.  This might be an
automatic mechanism which uses monitoring or logging to signal
hints to the operator.

## About Prime Numerals

Most examples of this sort of system use modulo values which
are prime numerals.  This is not a strict requirement, but it
is a lazy habit because it often works well.

The *Smallest Common Multiple* is the general thing that we
need in the scenario sketched here, and it will work for any
values of N that we could setup.  Going from 12 nodes to 15
is no problem, in spite of the common factor 3.

The scattering of data fragments over a cluster may improve
when the number of nodes and the number of database splits
is *relative prime*, meaning that they have no common
factor, or that their *greatest common divider* is 1.  But
this is not in any way an exact law.  The sort of thing
that might happen is that, say, every other system is
full, empty, full, empty, ... &mdash; which we should
avoid.

A pattern that is often seen is that one factor is a power
of two and the other is a prime numeral.  When the prime is
not 2, the factors will be relative prime and they can
grow independently within their numeric constraint.
