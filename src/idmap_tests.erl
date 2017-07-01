% Tests for the idmap module

-module( idmap_tests ).

-export([
	testdb/0
]).

%% Incorporate EUnit test macros
-include_lib( "eunit/include/eunit.hrl" ).

%% Setup a test database
testdb() ->
	% pseudonymTab
	Ps = ets:new( undefined, [
			set,
			public,
			{read_concurrency,true}
			] ),
	ets:insert( Ps, { { <<"jonathan">>, <<"example.com">> },
			[ <<"john">>, <<"johann">> ] } ),
	ets:insert( Ps, { { <<"meridith">>, <<"example.org">> },
			[ <<"mary">> ] } ),
	% aliasTab
	Ali = ets:new( undefined, [
			set,
			public,
			{read_concurrency,true}
			] ),
	ets:insert( Ali, { { <<"john">>, <<"example.com">> },
			[ <<"sales">>, <<"hunter">> ] } ),
	ets:insert( Ali, { { <<"jonathan">>, <<"example.com">> },
			[ <<"racer">> ] } ),
	ets:insert( Ali, { { <<"johann">>, <<"example.com">> },
			[ <<"chef">> ] } ),
	ets:insert( Ali, { { <<"mary">>, <<"example.org">> },
			[ <<"cook">> ] } ),
	% flockTab
	Flo = ets:new( undefined, [
			set,
			public,
			{read_concurrency,true}
			] ),
	ets:insert( Flo, { { <<"john">>, <<"sales">>, <<"example.com">> },
			[ { group, <<"sales">>, <<"john">> },
			  { role, <<"seller">>, <<"john">> },
			  { role, <<"taster">>, <<"john">> } ] } ),
	ets:insert( Flo, { { <<"john">>, <<"cook">>, <<"example.com">> },
			[ { role, <<"taster">>, <<"john">> } ] } ),
	ets:insert( Flo, { { <<"john">>, <<"cook">>, <<"example.com">> },
			[ { role, <<"taster">>, <<"john">> } ] } ),
	ets:insert( Flo, { { <<"mary">>, <<"cook">>, <<"example.org">> },
			[ { role, <<"taster">>, <<"mary">>, <<"example.com">> } ] } ),
	% The database returned is a map from label to table
	#{ aliasTab => Ali, pseudonymTab => Ps, flockTab => Flo }.

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

%% Test the access/3 function.
%%
access_test_ () ->
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
	_Taster = { <<"taster">>, absent, current },
	Taster_john = { <<"taster">>, <<"john">>, current },
	Taster_mary_com = { <<"taster">>, <<"mary">>, <<"example.com">> },
	_Seller = { <<"seller">>, absent, current },
	Seller_john = { <<"seller">>, <<"john">>, current },
	Sales_john = { <<"sales">>, <<"john">>, current },
	[ ?_assertEqual( Out,idmap:access(Db,Dom,In)) || {Dom,In,Out} <- [
		{ Com,John_com,[Taster_john,Seller_john,Sales_john,John_hunter,John_sales,John] },
		{ Com,John,[Taster_john,Seller_john,Sales_john,John_hunter,John_sales,John] },
		{ Org,John,[John] },
		{ Org,John_com,[John_com] },
		{ Org,Mary,[Taster_mary_com,Mary_cook,Mary] },
		{ Org,Johann_com,[Johann_com] },
		{ Com,Johann_com,[Johann_chef,Johann] },
		{ Com,Mary,[Mary] },
		{ Com,Mary_org,[Mary_org] },
		{ Com,Mary_com,[Mary] },
		{ Com,Jonathan,[Jonathan_racer,Jonathan] },
		{ Org,Meridith,[Meridith] },
		{ Org,Meridith_org,[Meridith] },
		{ Com,Meridith,[Meridith] }
	] ].

