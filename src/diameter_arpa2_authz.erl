%% -------------------------------------------------------------------
%% This is a generated file.
%% -------------------------------------------------------------------

-module(diameter_arpa2_authz).

-compile({parse_transform, diameter_exprecs}).

-compile(nowarn_unused_function).

-export_records(['Query', 'Response']).

-record('Query',
	{'Session-Id', 'Destination-Realm', 'User-Name',
	 'User-Password', 'NAS-Port' = [], 'NAS-Identifier' = [],
	 'Route-Record' = []}).

-record('Response',
	{'Session-Id', 'Result-Code', 'User-Name' = [],
	 'Filter-Id' = [], 'Route-Record' = []}).

-export([name/0, id/0, vendor_id/0, vendor_name/0,
	 decode_avps/2, encode_avps/2, msg_name/2, msg_header/1,
	 rec2msg/1, msg2rec/1, name2rec/1, avp_name/2,
	 avp_arity/2, avp_header/1, avp/3, grouped_avp/3,
	 enumerated_avp/3, empty_value/1, dict/0]).

-include_lib("diameter/include/diameter.hrl").

-include_lib("diameter/include/diameter_gen.hrl").

name() -> diameter_arpa2_authz.

id() -> 3.

vendor_id() -> erlang:error(undefined).

vendor_name() -> erlang:error(undefined).

msg_name(287, true) -> 'Query';
msg_name(287, false) -> 'Response';
msg_name(_, _) -> ''.

msg_header('Query') -> {287, 192, 3};
msg_header('Response') -> {287, 64, 3};
msg_header(_) -> erlang:error(badarg).

rec2msg('Query') -> 'Query';
rec2msg('Response') -> 'Response';
rec2msg(_) -> erlang:error(badarg).

msg2rec('Query') -> 'Query';
msg2rec('Response') -> 'Response';
msg2rec(_) -> erlang:error(badarg).

name2rec(T) -> msg2rec(T).

avp_name(283, undefined) ->
    {'Destination-Realm', 'DiameterIdentity'};
avp_name(11, undefined) -> {'Filter-Id', 'UTF8String'};
avp_name(32, undefined) ->
    {'NAS-Identifier', 'UTF8String'};
avp_name(5, undefined) -> {'NAS-Port', 'Unsigned32'};
avp_name(268, undefined) ->
    {'Result-Code', 'Unsigned32'};
avp_name(282, undefined) ->
    {'Route-Record', 'DiameterIdentity'};
avp_name(263, undefined) ->
    {'Session-Id', 'UTF8String'};
avp_name(1, undefined) -> {'User-Name', 'UTF8String'};
avp_name(2, undefined) ->
    {'User-Password', 'OctetString'};
avp_name(_, _) -> 'AVP'.

avp_arity('Query', 'Session-Id') -> 1;
avp_arity('Query', 'Destination-Realm') -> 1;
avp_arity('Query', 'User-Name') -> 1;
avp_arity('Query', 'User-Password') -> 1;
avp_arity('Query', 'NAS-Port') -> {0, 1};
avp_arity('Query', 'NAS-Identifier') -> {0, 1};
avp_arity('Query', 'Route-Record') -> {0, '*'};
avp_arity('Response', 'Session-Id') -> 1;
avp_arity('Response', 'Result-Code') -> 1;
avp_arity('Response', 'User-Name') -> {0, 1};
avp_arity('Response', 'Filter-Id') -> {0, 1};
avp_arity('Response', 'Route-Record') -> {0, '*'};
avp_arity(_, _) -> 0.

avp_header('Destination-Realm') -> {283, 64, undefined};
avp_header('Filter-Id') -> {11, 64, undefined};
avp_header('NAS-Identifier') -> {32, 64, undefined};
avp_header('NAS-Port') -> {5, 64, undefined};
avp_header('Result-Code') -> {268, 64, undefined};
avp_header('Route-Record') -> {282, 64, undefined};
avp_header('Session-Id') -> {263, 64, undefined};
avp_header('User-Name') -> {1, 64, undefined};
avp_header('User-Password') -> {2, 64, undefined};
avp_header(_) -> erlang:error(badarg).

avp(T, Data, 'Destination-Realm') ->
    diameter_types:'DiameterIdentity'(T, Data);
avp(T, Data, 'Filter-Id') ->
    diameter_types:'UTF8String'(T, Data);
avp(T, Data, 'NAS-Identifier') ->
    diameter_types:'UTF8String'(T, Data);
avp(T, Data, 'NAS-Port') ->
    diameter_types:'Unsigned32'(T, Data);
avp(T, Data, 'Result-Code') ->
    diameter_types:'Unsigned32'(T, Data);
avp(T, Data, 'Route-Record') ->
    diameter_types:'DiameterIdentity'(T, Data);
avp(T, Data, 'Session-Id') ->
    diameter_types:'UTF8String'(T, Data);
avp(T, Data, 'User-Name') ->
    diameter_types:'UTF8String'(T, Data);
avp(T, Data, 'User-Password') ->
    diameter_types:'OctetString'(T, Data);
avp(_, _, _) -> erlang:error(badarg).

enumerated_avp(_, _, _) -> erlang:error(badarg).

empty_value(Name) -> empty(Name).

dict() ->
    [1,
     {avp_types,
      [{"Destination-Realm", 283, "DiameterIdentity", "M"},
       {"Filter-Id", 11, "UTF8String", "M"},
       {"NAS-Identifier", 32, "UTF8String", "M"},
       {"NAS-Port", 5, "Unsigned32", "M"},
       {"Result-Code", 268, "Unsigned32", "M"},
       {"Route-Record", 282, "DiameterIdentity", "M"},
       {"Session-Id", 263, "UTF8String", "M"},
       {"User-Name", 1, "UTF8String", "M"},
       {"User-Password", 2, "OctetString", "M"}]},
     {avp_vendor_id, []}, {codecs, []},
     {command_codes, [{287, "Query", "Response"}]},
     {custom_types, []}, {define, []}, {enum, []},
     {grouped, []}, {id, 3}, {import_avps, []},
     {import_enums, []}, {import_groups, []}, {inherits, []},
     {messages,
      [{"Query", 287, ['REQ', 'PXY'], [],
	[{{"Session-Id"}}, {"Destination-Realm"}, {"User-Name"},
	 {"User-Password"}, ["NAS-Port"], ["NAS-Identifier"],
	 {'*', ["Route-Record"]}]},
       {"Response", 287, ['PXY'], [],
	[{{"Session-Id"}}, {{"Result-Code"}}, ["User-Name"],
	 ["Filter-Id"], {'*', ["Route-Record"]}]}]},
     {name, "diameter_arpa2_authz"}].


