# IdentityHub / Authorisation

> *The Authorisation component of the IdentityHub is used
> to decide whether an authenticated user may act as a
> requested authorisation user, and/or whether they may
> access a given resource.  Authentication is assumed to
> have taken place, this component is concerned about
> identity mapping and access control lists.*

IdentityHub Authorisation provides its service as Diameter
over SCTP.  Possible future extensions include additional
Diameter service over AMQP 1.0, which will enable clients
to override this component with their own.

For details, please visit the
[IdP Authorisation](http://idp.arpa2.net/authz.html)
home page in the ARPA2 project.  This also details the
Diameter model.


## Features

This component is written in Erlang.  This was not a light
choice, but it is perhaps the best language for this goal.
Authorisation should be rock-solid, possibly run in a
highly available mode as part of a cluster.  In addition,
it should be as fast as is possible (and as is secure).

Features of the current implementation are:

  * Operation orthogonal to authentication
  * Highly available through service replication
  * Scalable load, linear to number of service nodes
  * Scalable data size, linear to number of service nodes
  * Information fed over LDAP, using
    [SteamWorks Pulley](http://steamworks.arpa2.net/pulley.html)


## Local Data Caching and Redundancy

It is useful to understand the data storage model, and its
implications for message routing within an authorisation
cluster.

The authorisation data is summerised into two tables:

 1. An identity mapping table maps authenticated identities
    to authorisation identities
 2. An access control list table holds black and white lists
    for UUID-identified resources under domains

This data is derived from configuration in LDAP.  Specifically,
the identity map is a transitive closure of the
[identity inheritance](http://internetwide.org/blog/2016/12/18/id-6-inheritance.html)
configured for a domain.  And
[access control lists](http://donai.arpa2.net/acl.html)
implement the
[authorisation model](http://idp.arpa2.net/model.html)
that we wil use throughout ARPA2, basically stating a
black and white list with
[selectors](http://donai.arpa2.net/selector.html)
for local (and foreign) users.

The LDAP store used is a service store, from which we pull
data using
[SteamWorks Pulley](http://steamworks.arpa2.net/pulley.html).
This data is processed locally to a domain, but may still
expand into a large set of user name switches when frivolous
use of aliases, groups and roles is permitted.  Unfolding it
however, is useful in the interest of a responsive authorisation
service.

A vital design choice for the authorisation component was that
not all data needs to reside on each node of an authorisation
cluster.  Instead, the data is spread over the cluster based
on the domain name.  Cluster nodes each represent a set of
domain names, selected from a modulus of a hash over
the domain name.  There can be multiple cluster nodes for
each domain, so as to implement high availability.

Given domain data sized `D` and replication over `N` nodes
out of a total cluster size `C`, the amount of data for
each node averages around `D * N / C` &mdash; linear and
therefore quite scalable.  The only thing we need to take
care of is that requests are relayed to a domain node in
the cluster, which also happens to help to detect the need
for a fallback node.

This setup should lead to a resonable spread of the work load
over machines.  In a future version we plan to permit the
addition and removal of nodes servicing certain domains, to
manipulate this dynamically.

Any client can connect to any cluster node to request
authorisation over Diameter.  This client than orchestrates
the request, and sends a query to nodes holding the identity
map and the access control list; for domain-local uses, these
will normally be the same cluster node.  The outcome of these
two queries will be sent to a response-formulating node,
which ships back the Diameter response.

Failure of any node in this process is detected, and leads
to fallback to other nodes.  The one thing that remains, is
the client connection which too can fail.  Given that the
client is configured to reconnect and retry in such situations,
and assuming that it is setup with a list of authorisation
nodes, this should be rock-solid.  Note that AMQP can take
care of this even more dynamically &mdash; once a message is
enqueued, it can be pulled off by any live cluster node.
With AMQP, the option even arises to send the response from
another node than the one receiving the request, which is
not possible when using Diameter over SCTP.

