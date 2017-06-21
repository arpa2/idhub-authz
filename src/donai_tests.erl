%% Test module for donai.erl

-module(donai_tests).

%% Incorporate EUnit test macros
-include_lib("eunit/include/eunit.hrl").

good_donai_syntax_test_ () -> [
	?_assertEqual (donai:parse_fqn2adr (In), Out) || {In,Out} <- [
		{<<"john@example.com">>,
			{<<"john">>,absent,<<"example.com">>}},
		{<<"john+sales@example.com">>,
			{<<"john">>,<<"sales">>,<<"example.com">>}},
		{<<"john+top+sales@example.com">>,
			{<<"john">>,<<"top+sales">>,<<"example.com">>}},
		{<<"john@example">>,
			{<<"john">>,absent,<<"example">>}},
		{<<"john@example.com.">>,
			{<<"john">>,absent,<<"example.com.">>}}
	]
].

bad_donai_syntax_test_ () -> [
	?_assertEqual (syntax_error, donai:parse_fqn2adr (In)) || In <- [
		<<"john">>,
		<<"example.com">>,
		<<"john@example.com@example.net">>,
		<<"@example.com">>,
		<<"+@example.com">>,
		<<"john+@example.com">>,
		<<"+john@example.com">>,
		<<"john@.example.com">>,
		<<"john@.com">>,
		<<"john@.">>
	]
].

good_donai_selector_syntax_test_ () -> [
	?_assertEqual (donai:parse_fqnsel2adrsel (In), Out) || {In,Out} <- [
               {<<"john@example.com">>,
                       {<<"john">>,absent,<<"example.com">>}},
               {<<"john+sales@example.com">>,
                       {<<"john">>,<<"sales">>,<<"example.com">>}},
               {<<"john+top+sales@example.com">>,
                       {<<"john">>,<<"top+sales">>,<<"example.com">>}},
		{<<"@example.com">>,
			{anything,optional,<<"example.com">>}},
		{<<"+@example.com">>,
			{anything,anything,<<"example.com">>}},
		{<<"john+@example.com">>,
			{<<"john">>,anything,<<"example.com">>}},
		{<<"+sales@example.com">>,
			{anything,<<"sales">>,<<"example.com">>}},
		{<<"+top+sales@example.com">>,
			{anything,<<"top+sales">>,<<"example.com">>}},
		{<<"john@.example.com">>,
			{<<"john">>,absent,{subof,<<"example.com">>}}},
		{<<"john@.com">>,
			{<<"john">>,absent,{subof,<<"com">>}}},
		{<<"john@.">>,
			{<<"john">>,absent,anything}}
	]
].

bad_donai_selector_syntax_test_ () -> [
	?_assertEqual (syntax_error, donai:parse_fqn2adr (In)) || In <- [
		<<"">>,
		<<".">>,
		<<"john">>,
		<<"example.com">>,
		<<".example.com">>,
		<<"example.com.">>
	]
].

donai_match_selector_test_ () -> [
	?_assert (donai:compare_adrsel2adr (
				donai:parse_fqnsel2adrsel (Sel),
				donai:parse_fqn2adr (Adr))
			) || {Adr,Sel} <- [
		{<<"john@example.com">>,<<"john@example.com">>},
		{<<"john@example.com">>,<<"@example.com">>},
		{<<"john@example.com">>,<<"@.com">>},
		{<<"john@example.com">>,<<"john@.com">>},
		{<<"john@example.com">>,<<"@.">>},
		{<<"john@examepl.com">>,<<"john@.">>},
		{<<"john+sales@example.com">>,<<"john+sales@example.com">>},
		{<<"john+sales@example.com">>,<<"john+@example.com">>},
		{<<"john+sales@example.com">>,<<"+sales@example.com">>},
		{<<"john+sales@example.com">>,<<"+@example.com">>},
		{<<"john+sales@example.com">>,<<"john+sales@.com">>},
		{<<"john+sales@example.com">>,<<"john+sales@.">>},
		{<<"john+sales@example.com">>,<<"+@.">>},
		{<<"john+sales@example.com">>,<<"@.">>}
	]
].

donai_match_selector_curdom_test_ () ->
	Cur = <<"example.com">>, [
	?_assert (donai:compare_adrsel2adr (
				donai:parse_fqnsel2adrsel (Sel, Cur),
				donai:parse_fqn2adr (Adr, Cur),
				Cur)
			) || {Adr,Sel} <- [
		{<<"john@example.com">>,<<"john@example.com">>},
		{<<"john@example.com">>,<<"@example.com">>},
		{<<"john@example.com">>,<<"@.com">>},
		{<<"john@example.com">>,<<"john@.com">>},
		{<<"john@example.com">>,<<"@.">>},
		{<<"john@example.com">>,<<"john@.">>},
		{<<"john+sales@example.com">>,<<"john+sales@example.com">>},
		{<<"john+sales@example.com">>,<<"john+@example.com">>},
		{<<"john+sales@example.com">>,<<"+sales@example.com">>},
		{<<"john+sales@example.com">>,<<"+@example.com">>},
		{<<"john+sales@example.com">>,<<"john+sales@.com">>},
		{<<"john+sales@example.com">>,<<"john+sales@.">>},
		{<<"john+sales@example.com">>,<<"+@.">>},
		{<<"john+sales@example.com">>,<<"@.">>}
	]
].

donai_mismatch_selector_test_ () -> [
	?_assertNot (donai:compare_adrsel2adr (
				donai:parse_fqnsel2adrsel (Sel),
				donai:parse_fqn2adr (Adr))
			) || {Adr,Sel} <- [
		{<<"john@example.com">>,<<"john@.example.com">>},
		{<<"john@example.com">>,<<"+@example.com">>},
		{<<"john@example.com">>,<<"john+@example.com">>}
	]
].

donai_mismatch_selector_curdom_test_ () ->
	Cur = <<"example.com">>, [
	?_assertNot (donai:compare_adrsel2adr (
				donai:parse_fqnsel2adrsel (Sel, Cur),
				donai:parse_fqn2adr (Adr, Cur),
				Cur)
			) || {Adr,Sel} <- [
		{<<"john@example.com">>,<<"john@.example.com">>},
		{<<"john@example.com">>,<<"+@example.com">>},
		{<<"john@example.com">>,<<"john+@example.com">>}
	]
].


