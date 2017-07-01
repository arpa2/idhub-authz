% Tests for the xsmap module.

-module( xsmap_tests ).

-export([
	testdb/0
]).

%% Incorporate EUnit test macros
-include_lib( "eunit/include/eunit.hrl" ).

%% Setup a test database
testdb() ->
	Com = <<"example.com">>,
	Org = <<"example.org">>,
	DotXXX = {anything,optional,{subof,<<"xxx">>}},
	SalesAt = {<<"sales">>,optional,anything},
	UUID1 = <<"76CAF52F-2109-4EC7-9519-42B744833CD8">>,
	UUID2 = <<"76CAF52F-2109-4EC7-9519-42B744833CD86334E628-84EB-43FE-851D-A460A0F563F7">>,
	% UUID3 = <<"6334E628-84EB-43FE-851D-A460A0F563F7">>,
	% UUID4 = <<"0E214F42-22D7-465C-A53D-A5B433A99FC4">>,
	JohnLogin = {<<"john">>,absent,Com},
	MaryLogin = {<<"mary">>,absent,Org},
	JohnCook = {<<"john">>,<<"cook">>,Com},
	MaryCook = {<<"mary">>,<<"cook">>,Org},
	MarySoprano = {<<"mary">>,<<"soprano">>,Org},
	JohnAnyRole = {<<"john">>,anything,Com},
	CatchAll = {anything,optional,anything},
	Rsc = ets:new( undefined, [
			set,
			public,
			{read_concurrency,true}
			] ),
	ets:insert( Rsc, { { UUID1, Com },
			[ {10, black, [DotXXX,SalesAt] },
			  {20, white, [JohnCook] },
			  {30, gray,  [JohnAnyRole] },
			  {40, black, [CatchAll] } ] } ),
	ets:insert( Rsc, { { UUID2, Com },
			[ {15, black, [DotXXX,SalesAt] },
			  {25, white, [JohnCook] },
			  {35, white,  [JohnLogin] },
			  {45, black, [CatchAll] } ] } ),
	Coms = ets:new( undefined, [
			set,
			public,
			{read_concurrency,true}
			] ),
	ets:insert( Coms, { JohnCook,
			[ { 12, artisan, [MarySoprano,MaryCook]} ] } ),
	ets:insert( Coms, { JohnAnyRole,
			[ { 13, fountain, [DotXXX,MaryLogin] } ] } ),
	ets:insert( Coms, { JohnLogin,
			[ { 14, collegues, [MaryLogin] } ] } ),
	ets:insert( Coms, { MaryLogin,
			[ { 0, blockit, [SalesAt] },
			  { 1, welcome, [JohnAnyRole] },
			  { 2, eject_reject, [CatchAll] } ] } ),
	#{ resourceTab => Rsc, communicationTab => Coms }.

%% Test the resouce/4 function by comparing accessibility() listings.
%%
resource_test_ () ->
	Db = testdb(),
	Com = <<"example.com">>,
	DotXXX = {anything,optional,{subof,<<"xxx">>}},
	SalesAt = {<<"sales">>,optional,anything},
	UUID1 = <<"76CAF52F-2109-4EC7-9519-42B744833CD8">>,
	UUID2 = <<"76CAF52F-2109-4EC7-9519-42B744833CD86334E628-84EB-43FE-851D-A460A0F563F7">>,
	UUID3 = <<"6334E628-84EB-43FE-851D-A460A0F563F7">>,
	UUID4 = <<"0E214F42-22D7-465C-A53D-A5B433A99FC4">>,
	JohnCook = {<<"john">>,<<"cook">>,Com},
	JohnAnyRole = {<<"john">>,anything,Com},
	JohnLogin = {<<"john">>,absent,Com},
	CatchAll = {anything,optional,anything},
	[ ?_assertEqual( Out,xsmap:resource( Db,Com,UUID,done )) || {UUID,Out} <- [
		{UUID1, [ {10,black, [DotXXX,SalesAt] },
			  {20,white, [JohnCook] },
			  {30,gray,  [JohnAnyRole] },
			  {40,black, [CatchAll] } |
			  done ] },
		{UUID2, [ {15,black, [DotXXX,SalesAt] },
			  {25,white, [JohnCook] },
			  {35,white,  [JohnLogin] },
			  {45,black, [CatchAll] } |
			  done ] },
		{UUID3, done},
		{UUID4, done}
	] ].

%% Test the communicate/4 function by comparing accessibility() lists.
%%
communicate_test_ () ->
	Db = testdb(),
	Com = <<"example.com">>,
	Org = <<"example.org">>,
	DotXXX = {anything,optional,{subof,<<"xxx">>}},
	SalesAt = {<<"sales">>,optional,anything},
	% UUID1 = <<"76CAF52F-2109-4EC7-9519-42B744833CD8">>,
	% UUID2 = <<"76CAF52F-2109-4EC7-9519-42B744833CD86334E628-84EB-43FE-851D-A460A0F563F7">>,
	% UUID3 = <<"6334E628-84EB-43FE-851D-A460A0F563F7">>,
	% UUID4 = <<"0E214F42-22D7-465C-A53D-A5B433A99FC4">>,
	JohnLogin = {<<"john">>,absent,Com},
	MaryLogin = {<<"mary">>,absent,Org},
	JohnCook = {<<"john">>,<<"cook">>,Com},
	MaryCook = {<<"mary">>,<<"cook">>,Org},
	MarySoprano = {<<"mary">>,<<"soprano">>,Org},
	JohnAnyRole = {<<"john">>,anything,Com},
	CatchAll = {anything,optional,anything},
	[ ?_assertEqual( Out,xsmap:communication( Db,Com,Adr,silence )) || {Adr,Out} <- [
		{ JohnCook, [ { 12, artisan, [MarySoprano,MaryCook] } | silence ] },
		{ JohnAnyRole, [ { 13, fountain, [DotXXX,MaryLogin] } | silence ] },
		{ JohnLogin, [ { 14, collegues, [MaryLogin] } | silence ] },
		{ MaryCook, silence },
		{ MaryLogin, [ { 0, blockit, [SalesAt] },
                               { 1, welcome, [JohnAnyRole] },
		               { 2, eject_reject, [CatchAll] } | silence ] }
	] ].

%% Test the accessible/3 function by checking proper answers.
%%
accessible_test_ () ->
	% Db = testdb(),
	Com = <<"example.com">>,
	Org = <<"example.org">>,
	DotXXX = {anything,optional,{subof,<<"xxx">>}},
	SalesAt = {<<"sales">>,optional,anything},
	% UUID1 = <<"76CAF52F-2109-4EC7-9519-42B744833CD8">>,
	% UUID2 = <<"76CAF52F-2109-4EC7-9519-42B744833CD86334E628-84EB-43FE-851D-A460A0F563F7">>,
	% UUID3 = <<"6334E628-84EB-43FE-851D-A460A0F563F7">>,
	% UUID4 = <<"0E214F42-22D7-465C-A53D-A5B433A99FC4">>,
	JohnLogin = {<<"john">>,absent,Com},
	MaryLogin = {<<"mary">>,absent,Org},
	JohnCook = {<<"john">>,<<"cook">>,Com},
	MaryCook = {<<"mary">>,<<"cook">>,Org},
	MarySoprano = {<<"mary">>,<<"soprano">>,Org},
	JohnAnyRole = {<<"john">>,anything,Com},
	CatchAll = {anything,optional,anything},
	[ ?_assertEqual( Out,xsmap:accessible( Adr,Xs,kickout)) || {Adr,Xs,Out} <- [
		{[JohnCook], [ {10,black, [DotXXX,SalesAt] },
			  {20,white, [JohnCook] },
			  {30,gray,  [JohnAnyRole] },
			  {40,black, [CatchAll] },
			  {white,JohnCook} ] },
		{[MarySoprano], [ {15,black, [DotXXX,SalesAt] },
			  {25,white, [JohnCook] },
			  {35,white,  [JohnLogin] },
			  {45,black, [CatchAll] },
			  {black,MarySoprano} ] },
		{[MarySoprano,JohnCook], [ {15,black, [DotXXX,SalesAt] },
			  {25,white, [JohnCook] },
			  {35,white,  [JohnLogin] },
			  {45,black, [MarySoprano] },
			  {black,MarySoprano} ] },
		{[JohnCook,MarySoprano], [ {15,black, [DotXXX,SalesAt] },
			  {25,white, [JohnCook] },
			  {35,white,  [MarySoprano] },
			  {45,black, [CatchAll] },
			  {white,JohnCook} ] },
		{[MaryCook], [], {kickout,MaryCook}},
		{[MaryLogin], [], {kickout,MaryLogin}},
		{[JohnCook], [ { 12, artisan, [MarySoprano,MaryCook] } ], {kickout,JohnCook} },
		{[MaryCook], [ { 12, artisan, [MarySoprano,MaryCook] } ], {artisan,MaryCook} },
		{[JohnCook,MaryCook], [ { 12, artisan, [MarySoprano,MaryCook] } ], {artisan,MaryCook} },
		{[MaryCook,JohnCook], [ { 12, artisan, [MarySoprano,MaryCook] } ], {artisan,MaryCook} },
		{[JohnAnyRole], [ { 13, fountain, [DotXXX,MaryLogin] } ], {kickout,JohnAnyRole} },
		{[JohnLogin], [ { 14, collegues, [MaryLogin] } ], {kickout,JohnLogin} },
		{[MaryLogin], [ { 14, collegues, [MaryLogin] } ], {collegues,MaryLogin} },
		{[MaryLogin,JohnLogin], [ { 14, collegues, [MaryLogin] } ], {collegues,MaryLogin} },
		{[JohnLogin,MaryLogin], [ { 14, collegues, [MaryLogin] } ], {collegues,MaryLogin} },
		{[MaryCook], [], {kickout,MaryCook} },
		{[JohnCook], [], {kickout,JohnCook} },
		{[JohnCook,MaryCook], [], {kickout,JohnCook} },
		{[MaryCook,JohnCook], [], {kickout,MaryCook} },
		{[MaryLogin,JohnLogin], [ { 0, blockit, [SalesAt] },
                               { 1, welcome, [JohnAnyRole] },
		               { 2, eject_reject, [CatchAll] } ], {eject_reject,MaryLogin} },
		{[MaryLogin,JohnLogin,JohnCook], [ { 0, blockit, [SalesAt] },
                               { 1, welcome, [JohnAnyRole] },
		               { 2, eject_reject, [CatchAll] } ], {welcome,JohnCook} }
	] ].


