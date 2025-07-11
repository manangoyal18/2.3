%% src/game_registry.erl
-module(game_registry).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
    register_game/2,
    get_game_pid/1,
    unregister_game/1,
    add_player/2,
    get_players/1
]).

-define(TABLE, game_registry).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ets:new(game_registry, [named_table, public, set, {keypos, 1}]),
    %ets:new(?TABLE, [named_table, public, set]),
    {ok, #{}}.

%% Register game by ID
register_game(GameId, Pid) ->
    ets:insert(game_registry, {GameId, Pid, []}),
    ok.

%% Lookup
%get_game_pid(GameId) ->
%    case ets:lookup(?TABLE, GameId) of
%        [{_, Pid}] -> {ok, Pid};
%        [] -> {error, not_found}
%    end.

get_game_pid(GameId) ->
    case ets:lookup(game_registry, GameId) of
        [{_, GamePid, _}] -> {ok, GamePid};
        [] -> {error, not_found}
    end.

add_player(GameId, PlayerPid) ->
    case ets:lookup(game_registry, GameId) of
        [{GameId, GamePid, Players}] ->
            NewPlayers = lists:usort([PlayerPid | Players]),
            ets:insert(game_registry, {GameId, GamePid, NewPlayers}),
            ok;
        [] ->
            {error, not_found}
    end.


get_players(GameId) ->
    case ets:lookup(game_registry, GameId) of
        [{_, _, Players}] ->
            Players;
        [] ->
            []
    end.


%get_game_pid(_) -> {error, invalid_game_id}.

%% Unregister
unregister_game(GameId) ->
    ets:delete(?TABLE, GameId),
    ok.

handle_call(_, _, State) -> {reply, ok, State}.
handle_cast(_, State) -> {noreply, State}.
handle_info(_, State) -> {noreply, State}.
terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.
