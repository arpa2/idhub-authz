%% Domain / Network Access Identifier
%%
%% These are the routines for parsing, comparing and otherwise processing
%% The DoNAI that we use as basic identifiers in the ARPA2 project, so
%% domain and user@domain; for internal interpretation conventions, we
%% distinguish the the form user+label@domain.
%%
%% In addition to these forms, we also accept patterns that we have called
%% DoNAI Selector form.  This form is used in Access Control Lists, namely
%% in black and white lists.
%%
%% We use binary() representations where we can; most addresses and
%% parts thereof are < 64 bytes, meaning they can be efficiently stored
%% by Erlang.  Since we may have a lot of data to store, this is important.
%% Longer identifiers are still no problem for Erlang in binary() form,
%% but are simply a little less efficient.
%%
%% http://donai.arpa2.net
%%
%% From: Rick van Rein <rick@openfortress.nl>


%%
%% DATA TYPE DEFINITIONS
%%

%% Declarations of the fqn ::= uid+lab@dom types.
-type uid()    :: binary().
-type lab()    :: binary() | absent.
-type domcur() :: binary().
-type dom()    :: domcur() | current.
-type fqn()    :: binary().
-type adr()    :: { uid(), lab(), dom() }.

%% Selector parts fqnsel ::= uidsel+labsel@domsel
%% Note that domsel now includes the .domain form
-type uidsel() :: uid() | anything.
-type labsel() :: lab() | anything | optional.
-type domsel() :: dom() | anything | { subof, dom() }.
-type fqnsel() :: binary().
-type adrsel() :: { uidsel(), labsel(), domsel() }.


