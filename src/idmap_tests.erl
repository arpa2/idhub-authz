% Tests for the idmap module

-module(idmap_tests).

-export([testdb/0]).

%% Incorporate EUnit test macros
-include_lib("eunit/include/eunit.hrl").

%% Setup a test database
testdb () ->
	% The tables of the idmap database
	Ali = ets:new( undefined, [
			set,
			public,
			{read_concurrency,true}
			] ),
	Ps = ets:new( undefined, [
			set,
			public,
			{read_concurrency,true}
			] ),
	% Test entries in the database
	ets:insert( Ps, { { <<"jonathan">>, <<"example.com">> },
			[ <<"john">>, <<"johann">> ] } ),
	ets:insert( Ps, { { <<"meridith">>, <<"example.org">> },
			[ <<"mary">> ] } ),
	ets:insert( Ali, { { <<"john">>, <<"example.com">> },
			[ <<"sales">>, <<"hunter">> ] } ),
	ets:insert( Ali, { { <<"jonathan">>, <<"example.com">> },
			[ <<"racer">> ] } ),
	ets:insert( Ali, { { <<"johann">>, <<"example.com">> },
			[ <<"chef">> ] } ),
	ets:insert( Ali, { { <<"mary">>, <<"example.org">> },
			[ <<"cook">> ] } ),
	% The database is a map from label to table
	#{ aliasTab => Ali, pseudonymTab => Ps }.

%TODO% Db = idmap_tests:testdb().
%TODO% dmap:impose( Db,<<"example.com">>,{<<"john">>,absent,current} ).
%TODO% idmap:impose( Db,<<"example.com">>,{<<"jonathan">>,absent,current} ).
%TODO% idmap:impose( Db,<<"example.com">>,{<<"jonathan">>,absent,current} ).

%% Test the impose/3 function with the various mappings they offer.
%% Identities like mary@example.com do not exist, but given that they are
%% provided under the assumption that they do, they are echoed in the reply.
%% This is intentional -- the impose/3 function has no check on existence
%% any downright assumes that the identity has truly been authenticated.
%%
imposter_test_() ->
	Db = testdb(),
	Com = <<"example.com">>,
	Org = <<"example.org">>,
	John = { <<"john">>,absent,current },
	John_com = { <<"john">>,absent,<<"example.com">> },
	John_hunter = { <<"john">>,<<"hunter">>,current },
	John_sales = { <<"john">>,<<"sales">>,current },
	Jonathan = { <<"jonathan">>,absent,current },
	_Jonathan_com = { <<"jonathan">>,absent,<<"example.com">> },
	Jonathan_racer = { <<"jonathan">>,<<"racer">>,current },
	Johann = { <<"johann">>,absent,current },
	Johann_com = { <<"johann">>,absent,<<"example.com">> },
	Johann_chef = { <<"johann">>,<<"chef">>,current },
	Mary = { <<"mary">>,absent,current },
	Mary_org = { <<"mary">>,absent,<<"example.org">> },
	Mary_com = { <<"mary">>,absent,<<"example.com">> },
	Mary_cook = { <<"mary">>,<<"cook">>,current },
	Meridith = { <<"meridith">>,absent,current },
	Meridith_org = { <<"meridith">>,absent,<<"example.org">> },
	[ ?_assertEqual (Out,idmap:impose (Db,Dom,In)) || {Dom,In,Out} <- [
		{ Com,John_com,[John_hunter,John_sales,John] },
%MUSTFAIL% { Com,John_com,[Meridith,John_sales,John] },
		{ Com,John,[John_hunter,John_sales,John] },
		{ Org,John,[John] },
		{ Org,John_com,[John_com] },
		{ Org,Mary,[Mary_cook,Mary] },
		{ Org,Johann_com,[Johann_com] },
		{ Com,Johann_com,[Johann_chef,Johann] },
		{ Com,Mary,[Mary] },
		{ Com,Mary_org,[Mary_org] },
		{ Com,Mary_com,[Mary] },
		{ Com,Jonathan,[Jonathan_racer,John_hunter,John_sales,Johann_chef,Johann,John,Jonathan] },
		{ Org,Meridith,[Mary_cook,Mary,Meridith] },
		{ Org,Meridith_org,[Mary_cook,Mary,Meridith] },
		{ Com,Meridith,[Meridith] }
	] ].

