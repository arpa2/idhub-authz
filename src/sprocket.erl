-module(sprocket).

-behaviour(gen_server).


-type user() :: binary().
-type alias() :: user().
-type pseudonym() :: user().
-type group() :: binary().
-type groupalias() :: alias().
-type member() :: group().
-type role() :: binary().
-type occupant() :: role().
-type rolealias() :: alias().
-type principal() :: user() | group().



init(_Arg) ->
	% init whatever
	{ ok, [] }.



loop(State) ->
	receive

	{ add_alias, User, Alias } -> ok;
	{ del_alias, User, Alias } -> ok;
	{ add_pseudonym, User, Pseudonym } -> ok;
	{ del_pseudonym, User, Pseudonym } -> ok;
	{ add_member, User, GroupAlias, Member, Group } -> ok;
	{ del_member, User, GroupAlias, Member, Group } -> ok;
	{ add_occupant, Principal, RoleAlias, Occupant, Role } -> ok;
	{ del_occupant, Principal, RoleAlias, Occupant, Role } -> ok

	end,
	State.

