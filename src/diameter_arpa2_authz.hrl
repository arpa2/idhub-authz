%% -------------------------------------------------------------------
%% This is a generated file.
%% -------------------------------------------------------------------

-hrl_name('diameter_arpa2_authz.hrl').


%%% -------------------------------------------------------
%%% Message records:
%%% -------------------------------------------------------

-record('Query',
	{'Session-Id', 'Destination-Realm', 'User-Name',
	 'User-Password', 'NAS-Port' = [], 'NAS-Identifier' = [],
	 'Route-Record' = []}).

-record('Response',
	{'Session-Id', 'Result-Code', 'User-Name' = [],
	 'Filter-Id' = [], 'Route-Record' = []}).
