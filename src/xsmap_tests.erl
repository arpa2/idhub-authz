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
			  {JohnCook,white} ] },
		{[MarySoprano], [ {15,black, [DotXXX,SalesAt] },
			  {25,white, [JohnCook] },
			  {35,white,  [JohnLogin] },
			  {45,black, [CatchAll] },
			  {MarySoprano,black} ] },
		{[MarySoprano,JohnCook], [ {15,black, [DotXXX,SalesAt] },
			  {25,white, [JohnCook] },
			  {35,white,  [JohnLogin] },
			  {45,black, [MarySoprano] },
			  {MarySoprano,black} ] },
		{[JohnCook,MarySoprano], [ {15,black, [DotXXX,SalesAt] },
			  {25,white, [JohnCook] },
			  {35,white,  [MarySoprano] },
			  {45,black, [CatchAll] },
			  {JohnCook,white} ] },
		{[MaryCook], [], {MaryCook,kickout}},
		{[MaryLogin], [], {MaryLogin,kickout}},
		{[JohnCook], [ { 12, artisan, [MarySoprano,MaryCook] } ], {JohnCook,kickout} },
		{[MaryCook], [ { 12, artisan, [MarySoprano,MaryCook] } ], {MaryCook,artisan} },
		{[JohnCook,MaryCook], [ { 12, artisan, [MarySoprano,MaryCook] } ], {MaryCook,artisan} },
		{[MaryCook,JohnCook], [ { 12, artisan, [MarySoprano,MaryCook] } ], {MaryCook,artisan} },
		{[JohnAnyRole], [ { 13, fountain, [DotXXX,MaryLogin] } ], {JohnAnyRole,kickout} },
		{[JohnLogin], [ { 14, collegues, [MaryLogin] } ], {JohnLogin,kickout} },
		{[MaryLogin], [ { 14, collegues, [MaryLogin] } ], {MaryLogin,collegues} },
		{[MaryLogin,JohnLogin], [ { 14, collegues, [MaryLogin] } ], {MaryLogin,collegues} },
		{[JohnLogin,MaryLogin], [ { 14, collegues, [MaryLogin] } ], {MaryLogin,collegues} },
		{[MaryCook], [], {MaryCook,kickout} },
		{[JohnCook], [], {JohnCook,kickout} },
		{[JohnCook,MaryCook], [], {JohnCook,kickout} },
		{[MaryCook,JohnCook], [], {MaryCook,kickout} },
		{[MaryLogin,JohnLogin], [ { 0, blockit, [SalesAt] },
                               { 1, welcome, [JohnAnyRole] },
		               { 2, eject_reject, [CatchAll] } ], {MaryLogin,eject_reject} },
		{[MaryLogin,JohnLogin,JohnCook], [ { 0, blockit, [SalesAt] },
                               { 1, welcome, [JohnAnyRole] },
		               { 2, eject_reject, [CatchAll] } ], {JohnCook,welcome} }
	] ].
