InternetWide Access Rights
==========================

>   *Given identities, we can start to assign Access Rights through
>   authorisation. What exactly are we going to provide for?*

We need a general idea of what Access Rights might be granted, and how this can
be computed by an Authorisation mechanism. The common models for Access Rights
make statements about the following actors:

-   **Actions** that are made; we capture those in the general notion of
    **Permissions**.

-   **Objects** to whom the actions are made; we also call those **Collections**
    or **Resources**.

-   **Subjects** that impose the actions on the objects; we express those
    through **Identities**.

Permissions
-----------

To retain consistency in identities presented, we shall need a consistent
ordering of permissions. This means that a higher level of permission implies
all the lower-level permissions. In order of dropping permission level, we have:

-   **Admins**, those with complete access to all objects (domain setting)

-   **Services**, those with regulated to work on objects (config setting)

-   **Deleters**, those who may delete objects (ACL)

-   **Creators**, those who may create objects (ACL)

-   **Writers**, those who may change objects (ACL)

-   **Readers**, those who may fetch data from objects (ACL)

-   **Provers**, those who can make inferences from objects (ACL)

-   **Knowers**, those who may know about the existence of objects (ACL)

-   **Owners**, those who may do anything to their owned objects (ACL)

-   **Visitors**, those without access privileges to the probed objects
    (fall-through)

TODO: *Who may edit permissions?*

Note that these are fairly general terms, but they can usually capture the sort
of privileges that a service desires. For instance, in the case of a phone call,
the writers may be used to represent speakers and the readers may be for people
listening in or recording. For a conference call, a writer would not imply the
right to tear it down, which would apply for a one-to-one call. The
interpretation is made by the services that decide to authorise certain actions.

Access Policies
---------------

The ACL properties are generally tied to an object. To allow some central
management of such settings, we may additionally have **Policies** that capture
standardised sets of permissions. It should be straightforward to combine the
outcomes from various policies by finding the highest Permission, and from the
identities offering that, pick a consistent one.

A very useful aspect of Policies is that they improve the impact of
authorisation caching. In this light, it is useful to note that the above
permissions cover the complete range from everything allowed to nothing allowed.
Missing permissions are therefore not cached, but not a sign of having no
permissions. This means that cached permissions can be removed when they may
have been invalidated by a change.

Caches may help to speed up authorisation searches, but the best is if they are
not spread over services. A central cache has a better hit rate. Moreover, for
audit logging it is useful to see authorisation inquiries come in repeatedly.
Even when something is found in an authorisation cache, it should still be
included in the audit log.

There may be a use for having Templates for some Policies; this would mean that
certain Variables can reference the context in which they materialise. It may be
more difficult to cache the conclusions from Templates, although when it works
it is also more generally usable. Variables could capture generic roles like
instances and names for self-references, as well as owners.

Authorisation Algorithm
-----------------------

Authorisation can run given an authenticated user identity and a desired
resource. It can step down to a multitude of identities; to support better
control over identities, the user may supply an additional user identity to
which it wants to step down.

1.  Given a step-down identity, require that the transition follows identity
    inheritance arrows.

    -   This is where having cached reachability is useful; path length does not
        matter.

2.  If the identity is setup under the domain as an admin, return the **admins**
    permission.

3.  If the identity is setup as a service, lookup its ACL. TODO: Applicable to
    which objects?

4.  Given a resource, lookup its ACL. TODO: TERM OBJECT INSTEAD OF RESOURCE?

5.  If we now have multiple ACLs, TODO:COMBINE?.

6.  Note that any empty ACL lists will grant no permission in the procedure
    below.

7.  Iterate over the **deleters**, **creators**, **writers**, **readers**,
    **provers**, **knowers**, **owners** and for each, check the authenticated
    identity against the ACL:

    1.  Consider all identities that are considered authenticated, following
        inheritance arrows.

    2.  Compare against the selector patterns in the ACL.

    3.  Given multiple answers, remove all but the ones with the shortest path
        from the authenticated identity.

        -   We can optimise by considering identities in groups, based on rising
            distance from the authenticated identity. Hmm, do we need to
            precompute that at all? Just need to avoid repeating previously
            checked identities.

    4.  Given multiple answers, pick a consistent one.

    5.  Given an answer, return the permission for the current ACL list.

8.  As a last resort, return the **visitors** permission.

### Consistency and Mentions of Identity

In the text above, we mentioned twice to *pick a consistent one* when faced with
multiple identities that return equally satisfying permissions when accessing a
resource.

The consistency is desired in the interest of mentioning identities, for
instance to mark a sender of a communication, or when audit logging. Audit logs
can be used for billing, but also for an administrator to see what has happened
in the past. It may however be practical to log just the Permissions granted
within the resource, for instance while it is somehow active. Since permission
levels imply all the permission levels below them, this is fruitful and it does
not reveal all the information desired for auditing purposes.

This means that we will permit the localised storage of Permissions for
logically related processes, and audit logging is generated whenever the
permissions are checked. This would be a function to take place in a library
that checks for permission. There may be some use in suppressing audits of
rejections of functionality that are not fatal, but merely determinants of a
course of action. For instance, when terminating a call leg, an inquiry into the
permission to delete the call can be silently ignored when the call is in fact a
conference that can continue without the presence of the call leg.

We can now formulate rules for consistent use of identities that are going to be
useful:

1.  In the interest of consistency among actions (and not having to request
    authorisation over and over again), it is useful if the same identity is
    used throughout. This means that the *highest permission* that can be
    reached should always prevail. This is why the authorisation algorithm
    addresses permissions from the highest level to the lowest.

2.  When the highest permission is granted from multiple identities, then the
    path length becomes a concern. The shorter paths improve consistency and the
    longer paths improve privacy. Since overlap expresses no meaning in terms of
    Access Control, and such facilities should not be overloaded to tap
    information about individuals, the possibility of improved auditing due to
    more elaborate ACLs is not taken considered an argument, except perhaps as
    one to avoid.But under fixed ACLs and identity inheritance, consistency is
    not endangered, so privacy is the winning argument, and so the *longest
    path* in the identity inheritance graph will be selected. TODO:CHOSEN, I
    THINK. CONCERNS AND THEIR RATHER LIGHT PREFERENCES ARE:

    -   Privacy: (LONG) paths loose the most of an identity’s specificity. But
        users can control it through step-down identities if they really want,
        so this is not a strict requirement for a privacy-supporting platform.

    -   Auditing: SHORT paths would retain most of the identity’s specificity,
        leading to more overlap in audited identities. But it might also to
        administrative practices with a lot of overlap in ACLs, which then
        become unnecessarily long and maintenance-intensive.

    -   Aptness/1: (SHORT) paths are less likely to pass through linked
        pseudonyms, and strive for other, unintended identities. That disturbs
        both privacy and consistency. This is however, a rather bad usage
        pattern; there are good reasons to discourage cycles and instead focus
        on hierarchical dissimination of identities, even if we connect
        pseudonyms. (We may draw pseudonyms separately in the diagram, and have
        them point to the same aliases with a `+` subname added.)

    -   Aptness/2: LONG paths are better when a “mother identity” can be avoided
        in logging, then this is better; and as long as no cycles are present in
        user identities, there will be no tendency to loop around for another
        identity just for the sake of finding the longest path.

    -   Aptness/3: LONG paths encourage running through cycles of group
        identities when they are recursively nested, which is idiotic but may
        nonetheless occur when they are managed by independent operators. This
        may put groups higher on the list of likely candidates relative to
        individuals, which is good, and the user is indeed part of all the
        groups in the cycle. Potential confusion only occurs when multiple
        groups in such recusive cycles are on the ACL, which is superfluous
        anyway. So there appears to be no problem with this. Cycles between
        groups are inconsiderate, but not problematic for consistency because
        the same primary identity will yield the same group membership across
        all objects approached.

    -   Overspec: (LONG) paths select a group match over a group member match.
        When both are present, then the ACL is a bit over-specified. You either
        want group members to identify themselves individually, or you don’t.

    -   Speed: SHORT paths arrive at their destination much faster, the search
        to go through is simpler and so is the derivation process.

3.  Given the longest path for the highest permission, there may still be more
    than one identity left to choose from. In this case, an arbitrary choice can
    be made, as long as it can be done consistently. It should suffice to look
    at just the identifiers themselves, so as to avoid database accesses in
    these trivial situations. We prefer the shortest domain name, if those are
    the same we prefer the shortest user name and if that still is the same we
    pick the first name according to comparison with POSIX' `strcasecmp()`
    function.

Note that special situations may call for delivering the list of available
identities. But that is more a concern at the user’s end (where the ACLs are
unknown, by the way). The one place where we expect user identities to be listed
is to potentially reach various object instances, such as various storage
containers, which does not call for the various identities that are able to
access a given Object.

Objects
-------

What an object is can vary dramatically, in response to the need to vary these
rights. A few examples are:

-   LDAP Object Classes

-   LDAP Attribute Types

-   LDAP Entries

-   LDAP Attribute Values

-   Service Types

-   Service Instances

-   Service Sessions

-   Resource Types

-   Resource Instances

For each of these, it may be sensible to attach an ACL, and/or one or more
Policy references. In some cases, the use of *variables* may be useful to come
to generic templates to be rolled out over their elements, instances or
realisations.

### LDAP Objects and Attributes

It may be possible to write out the authorisation constraints of the ACL and
possibly referenced Policies in terms of an ACI in objects themselves, inasfar
as they apply to individual entries and values, and in the server’s access
control mechanism inasfar as they apply to object classes and attribute types.

The access of concern is usually limited to reading and writing, although
creating, deleting are also used, and even proving and knowing are of possible
use in LDAP. We should keep in mind to be explicit about the administrators when
we do that.

### Services

Service Types are generic descriptions of services that have been incorporated
into a domain. The Service Instances describe the concrete places where they are
applied in that domain, and Service Sessions are dynamic run-time uses of those
Service Instances. Not all these variations need to be present for all Services.

The only access to Service Types is instantiation and perhaps later, deletion of
Service Instances. This can be regulated through an ACL. While instantiating, a
few *variables* or instantiation *parameters* may help to work out a Template to
form a true ACL for the Service Instance.

The creation and deletion of Service Sessions is the next stage, and rights to
do this could be included in the ACL for the Service Instance. Again, a Template
might be put to good use. Participants in the Session may provide *variables*
and choices when creating the Service Session may also deliver instantiation
*parameters*.

### Resources

The term Resource is used for things that a service may want to point at while
in action. Things like the concept of a mailbox would be a Resource, and a
particular mailbox would be a Resource Instance. It is possible to share
Resources and their Instances between Services as a form of communication
between them.
