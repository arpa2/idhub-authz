% authz_tests.erl -- Test module for authz.erl
%
% From: Rick van Rein <rick@openfortress.nl>

-module( authz_tests ).

%% Incorporate EUnit test macros
-include_lib( "eunit/include/eunit.hrl" ).


%% Generate a flockTab as a ready-to-merge database map
%%
testdb_flock() ->
	FT = ets:new( undefined, [
			set,
			public,
			{read_concurrency,true}
			] ),
	#{ flockTab => FT }.


%% Generate an aliasTab as a ready-to-merge database map
%%
testdb_alias() ->
	AT = ets:new( undefined, [
			set,
			public,
			{read_concurrency,true}
			] ),
	#{ aliasTab => AT }.


%% Test the impose_as/4 function.
%%
%% Instead of positive and negative tests, we simply assure success and
%% compare the resulting level -- which may be really low but not failure.
%%
impose_as_test_ () ->
	Db = idmap_tests:testdb(),
	Com = <<"example.com">>,
	Org = <<"example.org">>,
	John = { <<"john">>,absent,current },
	John_com = { <<"john">>,absent,<<"example.com">> },
	John_hunter = { <<"john">>,<<"hunter">>,current },
	John_sales = { <<"john">>,<<"sales">>,current },
	Jonathan = { <<"jonathan">>,absent,current },
	Jonathan_com = { <<"jonathan">>,absent,<<"example.com">> },
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
	AllIds = [ John_com, John_hunter, John_sales,
		Jonathan, Jonathan_com, Jonathan_racer,
		Johann, Johann_com, Johann_chef,
		Mary, Mary_org, Mary_com, Mary_cook,
		Meridith, Meridith_org ],
	OutFun = fun ( _Dom,AltId,OrigId,MappedIds ) ->
		case lists:member( AltId,MappedIds ) of
		true  -> {impose,  AltId};
		false -> {imposter,OrigId}
		end
	end,
	[ ?_assertEqual (OutFun(Dom,AltId,OrigId,MappedIds),authz:impose_as (Db,Dom,OrigId,AltId)) || {Dom,OrigId,MappedIds} <- [
		{ Com,John_com,[John_hunter,John_sales,John] },
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
	],
		AltId <- AllIds
	].


%% Test the resource_access/4 function.
%%
%% Instead of positive and negative tests, we simply assure success and
%% compare the resulting level -- which may be really low but not failure.
%%
resource_access_test_ () ->
	Db = maps:merge(
		xsmap_tests:testdb(),
		maps:merge(
			testdb_flock(),
			testdb_alias()
		)
	),
	Com = <<"example.com">>,
	Org = <<"example.org">>,
	_DotXXX = {anything,optional,{subof,<<"xxx">>}},
	_SalesAt = {<<"sales">>,optional,anything},
	SalesXXX = {<<"sales">>,absent,<<"se.xxx">>},
	UUID1 = <<"76CAF52F-2109-4EC7-9519-42B744833CD8">>,
	UUID2 = <<"76CAF52F-2109-4EC7-9519-42B744833CD86334E628-84EB-43FE-851D-A460A0F563F7">>,
	UUID3 = <<"6334E628-84EB-43FE-851D-A460A0F563F7">>,
	UUID4 = <<"0E214F42-22D7-465C-A53D-A5B433A99FC4">>,
	JohnLogin = {<<"john">>,absent,Com},
	MaryLogin = {<<"mary">>,absent,Org},
	JohnCook = {<<"john">>,<<"cook">>,Com},
	MaryCook = {<<"mary">>,<<"cook">>,Org},
	MarySoprano = {<<"mary">>,<<"soprano">>,Org},
	_JohnAnyRole = {<<"john">>,anything,Com},
	_CatchAll = {anything,optional,anything},
	AllIds = [SalesXXX,JohnLogin,MaryLogin,JohnCook,MaryCook,MarySoprano],
	ReduceLevel = fun( {Level,_} ) -> Level end,
	[ ?_assertEqual( Level,ReduceLevel( authz:resource_access( Db,Com,Id,UUID )) ) || {UUID,Level,Ids} <- [
		% Note: acl(UUID1) whitelists JohnAnyRole: "john+@", not "john@"
		% Note: This would have worked, had our aliasTab not been empty
		{ UUID1, black, [JohnLogin,SalesXXX,MaryLogin,MaryCook,MarySoprano] },
		{ UUID1, white, [JohnCook] },
		{ UUID1, gray,  [] },
		{ UUID1, blue,  [] },
		{ UUID2, black, [SalesXXX,MaryLogin,MaryCook,MarySoprano] },
		{ UUID2, white, [JohnLogin,JohnCook] },
		{ UUID2, blue,  [] },
		{ UUID3, blue,  [] },
		{ UUID4, blue,  [] },
		{ UUID3, black,  AllIds },
		{ UUID4, black,  AllIds }
	],
		Id <- Ids
	].


%% TODO: Test the communication_access/4 function.
%%
%% Instead of positive and negative tests, we simply assure success and
%% compare the resulting level -- which may be really low but not failure.
%%
communication_access_test_ () ->
	Db = maps:merge(
		xsmap_tests:testdb(),
		maps:merge(
			testdb_flock(),
			testdb_alias()
		)
	),
	Com = <<"example.com">>,
	Org = <<"example.org">>,
	_DotXXX = {anything,optional,{subof,<<"xxx">>}},
	_SalesAt = {<<"sales">>,optional,anything},
	SalesXXX = {<<"sales">>,absent,<<"se.xxx">>},
	% UUID1 = <<"76CAF52F-2109-4EC7-9519-42B744833CD8">>,
	% UUID2 = <<"76CAF52F-2109-4EC7-9519-42B744833CD86334E628-84EB-43FE-851D-A460A0F563F7">>,
	% UUID3 = <<"6334E628-84EB-43FE-851D-A460A0F563F7">>,
	% UUID4 = <<"0E214F42-22D7-465C-A53D-A5B433A99FC4">>,
	JohnLogin = {<<"john">>,absent,Com},
	MaryLogin = {<<"mary">>,absent,Org},
	JohnCook = {<<"john">>,<<"cook">>,Com},
	MaryCook = {<<"mary">>,<<"cook">>,Org},
	MarySoprano = {<<"mary">>,<<"soprano">>,Org},
	_JohnAnyRole = {<<"john">>,anything,Com},
	_CatchAll = {anything,optional,anything},
	AllIds = [SalesXXX,JohnLogin,MaryLogin,JohnCook,MaryCook,MarySoprano],
	ReduceLevel = fun( {Level,_} ) -> Level end,
	RecptDom = fun( {_Uid,_Lab,Dom} ) -> Dom end,
	[ ?_assertEqual( Level,ReduceLevel( authz:communication_access( Db,RecptDom( Recipient ),Sender,Recipient )) ) || {Recipient,Level,Senders} <- [
		{SalesXXX,gray,AllIds},
		{JohnLogin,collegues,[MaryLogin]},
		{JohnLogin,gray,[SalesXXX,JohnLogin,JohnCook,MaryCook,MarySoprano]},
		{MaryLogin,blockit,[SalesXXX]},
		{MaryLogin,welcome,[JohnCook]},
		{MaryLogin,eject_reject,[JohnLogin,MaryCook,MarySoprano]},
		{MaryLogin,gray,[]},
		{JohnCook,artisan,[MarySoprano,MaryCook]},
		% Not JohnAnyRole: {JohnCook,fountain,[SalesXXX,MaryLogin]}
		{JohnCook,gray,[JohnLogin,JohnCook]},
		{MaryCook,gray,AllIds},
		{MarySoprano,gray,AllIds}
	],
		Sender <- Senders
	].


