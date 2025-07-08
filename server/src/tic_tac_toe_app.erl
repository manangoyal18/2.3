-module(tic_tac_toe_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', tic_tac_toe_router:routes()}
    ]),
    {ok, _} = cowboy:start_clear(
        tic_tac_toe_http_listener,
        [{port, 8088}],
        #{env => #{dispatch => Dispatch}}
    ),

    %ets:new(game_registry, [named_table, public, set, {keypos, 1}]).


    tic_tac_toe_sup:start_link().

stop(_State) ->
    ok.