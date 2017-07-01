%% Access Controller for ARPA2 IdentityHub Authorisation
%%
%% This process answers "May this user use this Resource?"
%%
%% From: Rick van Rein <rick@openfortress.nl>


%% The "xsmap" database holds compiled Access Control Lists to resources.
%%
%% Resources may also be a marker for a particular service, such as a
%% web server.  In this case, a separate list is available for various
%% levels of access, and usually the most empowering level is desired.
%% (The access levels have a complete order in terms of empowering.)
%% See doc/authz-internal-design-spec.md for details on the levels.
%%
%% In the implementation in this module, it is assumed that ACLs are
%% sorted in the order of declining access level to achieve the highest
%% possible levels; but this is not the only possibility.  In other
%% words, such expectancies are implemented in the software driving
%% ACLs into this xsmap module.
%%
%% Communication models the ability to chat, using any protocol, with
%% the users of a domain.  This is established with black, white and
%% gray lists, and the communication request is reported as one of
%% these levels.
%%
%% In all cases, it is assumed that the requester will interpret the
%% reported grant level, and probably retain the data for future use
%% in the same session with an authenticated user.  It remains up to
%% the application protocol to fence off a session.  One might say that
%% every portal to internal functions, every service as viewed from
%% the "outside" network, is a Network Access Server in terms of the
%% Diameter and RADIUS protocols.
%%
%% The terms of the "xsmap" model holds on to ACLs in a compiled form, to
%% aid efficient handling.  The first list to provide a match determines
%% the outcome of the operation; this is possible because all available
%% outcomes can be setup explicitly with their own Access Control List.


%% The levels that can be assigned to communication / resources
%%
-type level_com() :: white | gray | black.
-type level_res() :: admin | service |
                     delete | create | write | read |
                     prove | know | own |
                     visit.
-type level()     :: level_com() | level_res().

%% Resources will be identified through a UUID in binary form with
%% uuid(), or in a concetation of one or two of these forms into
%% one binary with the same type.
%%
-type uuid() :: binary().

%% Client-supplied line numbers for ACLs help to insert, edit and delete ACLs
%%
-type lineno() :: integer().

%% Access Control Lists hold DoNAI Selectors for a given level
%%
-type acl() :: [ adrsel() ].

%% Accessibility defines the database format, namely a list of
%% triples of each a line number, level and ACL
%%
-type accessibility() :: [{lineno(),level(),acl()}].

