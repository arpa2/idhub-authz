ARPA2: Two Kinds of Authorisation
=================================

>   *On the Internet, at least as viewed from the ARPA2 perspective, there are
>   two kinds of authorisation that play an important role.  Meet them below.*

Authorisation is the question that starts from an authenticated/assured
identity, and asks the question “is that person permitted to do X?” — where X is
whatever action you have in mind.

Authorisation for Communication
-------------------------------

The first type of authorisation that we are interested in, is communication.  As
examples, consider sending an email or, in a modern variation, passing an object
through AMQP.  Or consider chatting with a person or group.

In general, we consider communication to be the exchange of information as it
could have crossed the boundaries of a realm (such as an Internet domain) —
could have, because the same mechanisms may also apply internally.

Communication is done from one identified endpoint to another identified
endpoint — where the endpoints are usually users or domains, but may in fact be
other actors, like phone numbers or perhaps even ISBN or EAN numbers.

Where communication may crossover between realms, we employ authorisation
through black and white lists.  More specifically:

-   Black and white lists are protocol agnostic.  Once you accept traffic over
    email, you also accept chat and telephony.  This does not imply access to a
    given internal resource, but it does mean that communication is welcomed
    that could try to get at it.

-   There are a few levels of defaults that may be applied.  A realm (such as a
    domain name) may define a default, which may be overridden when an endpoint
    (such as a user) defines its own black and/or white lists.

-   When only a black list is defined, it is assumed that the default is to pass
    traffic that is not black-listed.

-   When only a white list is defined, it is assumed that the default is to
    block traffic that is not white-listed.

-   When neither white nor black lists are defined, it is assumed that a more
    general default applies; lacking that, all communication is gray-listed.

-   When both black and white lists are defined, the most concrete match of the
    two decides on acceptance (if the white list gives the most concrete match)
    or rejection (if the black list gives the most concrete match).

-   When black and white lists are both defined, and both match a prospective
    remote identity at the same abstraction level, then the entry is considered
    gray-listed.

-   Gray-listing means that the local user will be asked for approval, following
    a policy that may be set for requesting approval; this may range from
    rejecting all requests, through asking for an XMPP link, to accepting every
    request; while at it, the attempted traffic will be put on hold and
    eventually rejected within the constraints of the protocol.

Authorisation for Resource Access
---------------------------------

Communication with the outside World generally happens over Queues such as ATOM
feeds or mail submission pathways.  This is in contrast to the management of
resources that are kept, more or less statically, within a realm.

Access to Resources often comes down to the right to read, write, create and
delete resources, the so-called CRUD access rights.  Under the InternetWide
Architecture, these rights are not generally granted to external actors, but
only to internal users — although some foreign users may be incorporated into
groups, be made occupants of a role, or be setup with a foreign-reference
userid.

Resource access is a different matter altogether:

-   Users may knock on the internal doors with authentication under a local user
    identity, but then step down to a more specific user identity, such as a
    group of which they are a member; this needs to be authorised before
    continuing under that new user identity.

-   The (effective) user identity can then be used to access a resource, such as
    the mailbox (for the identity) or the chatting channel (for the resource).
    Whether this is possible or, more accurately, what set of privileges is
    available during access, is the next authorisation inquiry to make.

Resources are described, as much as possible, at the level of a class — rather
than at the level of an instance.  So, there are no separate resource
descriptors for each mailbox, but rather for the general idea of a user’s
mailbox in general.  This helps to keep the pressure off the service definition
and keeps service implementations more static.  We will probably put up a
registry with general service definitions, perhaps in the form of mildly
structured text strings or stringently structured object identifiers.  If need
be, either of these forms could support additional embellishments with such
things as partitioning indications of variation and/or instance specifics.

External Access to Resources
----------------------------

In the first form, the authorisation query asks “can `user_A` pose as `user_B`
and communicate with `user_C`?” whereas in the second form, the central concern
is “can user_A pose as `user_B` and access `resource_R`, and if so, under what
`modes_M`?”.

Althoug the first form refers to an internal `user_B` and the second to an
external `user_B`, the use of the [DoNAI form](http://donai.arpa2.net) renders
the distinction moot, since both are taken from the same universe of
identifiers.  It is therefore possible to combine the two questions, and wonder
whether a `user_A` can change its identity to that of `user_B` and continue to
access `resource_R` under certain `modes_M`.  This is very helpful in permitting
external users to partake in one’s domain activity.

The [Diameter form](http://idp.arpa2.net/radius.html) of authorisation inquiries
plays into this potential combination:

-   The `User-Password` attribute always represents `user_A`; this may or may
    not include a domain part after a `@` symbol.  If none is supplied, then
    the `Destination-Realm` is appended after an `@` symbol.

-   The `User-Name` attribute always holds `user_B`, to which authorisation appends
    an `@` symbol and the `Destination-Realm`.  The result may simply be the same
    as `user_A`, which would be trivially granted.

-   The `Destination-Realm` attribute is always supplied, and indicates the
    targeted realm.  The addition of this attribute means that authorisation
    can be setup as a multi-tenant service.

-   When supplied, the `NAS-Port` attributes represents a `user_C`, and authorisation only
    succeeds when `user_B` may communicate with `user_C`

-   When supplied, the resource in `NAS-Identifier` represents a resource, and
    authorisation tells us in `Filter-ID` what access rights `user_B` has

TODO: Really allow this mixture, and really user 3 users, and really use the 2nd
to the resource?
