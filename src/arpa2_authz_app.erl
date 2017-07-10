-module(arpa2_authz_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    ok. #ch_sup:start_link().

stop(_State) ->
    ok.
