{application, arpa2_authz, [
	{description, "ARPA2 Authorisation Component for the IdentityHub in the InternetWide Architecture"},
	{mod, arpa2_authz_app,[]},
	{vsn, "0"},
	{modules, [donai,idmap,xsmap,authz,diameter_arpa2_authz]},
	{registered, []},	% no registered process names
	{applications, [kernel,stdlib,diameter]}
]}.

