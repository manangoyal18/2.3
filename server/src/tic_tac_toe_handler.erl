-module(tic_tac_toe_handler).
-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).
-export([handle_command/2]).

-include_lib("kernel/include/logger.hrl").
-include("tic_tac_toe_game.hrl").

%%% Entry point
init(Req, State) ->
    {cowboy_websocket, Req, State, #{idle_timeout => 600000}}.

%%% WebSocket initialization
websocket_init(_State) ->
    ?LOG_INFO("Client connected via WebSocket"),
    {ok, #{}}.

%%% Handle incoming WebSocket messages
websocket_handle({text, Msg}, State) ->
    io:format("Received message: ~s~n", [Msg]),
    try
        Parsed = jsone:decode(Msg, [{object_format, map}]),
        handle_command(Parsed, State)
    catch
        _:_Reason ->
            {reply, {text,jsone:encode(#{"error" => "Invalid message format"})}, State}
    end;
websocket_handle(_, State) ->
    {ok, State}.

%%% WebSocket internal messages
websocket_info({send, Payload}, State) ->
    {reply, {text, Payload}, State};
websocket_info(_, State) ->
    {ok, State}.

%%% WebSocket termination
terminate(_Reason, _Req, _State) ->
    io:format("WebSocket closed: ~p~n", [_Reason]),
    ok.

%%% Handle commands from clients
handle_command(#{"type" := "start_game"}, State) ->
    {ok, GamePid} = tic_tac_toe_game:start(),
    GameId = erlang:pid_to_list(GamePid),
    {ok, GameState} = tic_tac_toe_game:get_state(GamePid),
    Payload = jsx:encode(#{
        "type" => "game_state",
        "status" => GameState#state.status,
        "board" => GameState#state.board,
        "current_player" => GameState#state.current_player,
        "winner" => case GameState#state.winner of undefined -> "undefined"; W -> W end,
        "game_id" => list_to_binary(GameId),
        "your_symbol" => "x"
    }),
    self() ! {send, Payload},
    {ok, State#{game_pid => GamePid, game_id => GameId, symbol => "x"}};

handle_command(#{"type" := "join_game", "game_id" := GameIdBin}, State) ->
    GameIdStr = binary_to_list(GameIdBin),
    try
        GamePid = list_to_pid(GameIdStr),
        {ok, GameState} = tic_tac_toe_game:get_state(GamePid),
        Payload = jsx:encode(#{
            "type" => "game_state",
            "status" => GameState#state.status,
            "board" => GameState#state.board,
            "current_player" => GameState#state.current_player,
            "winner" => case GameState#state.winner of undefined -> "undefined"; W -> W end,
            "game_id" => GameIdBin,
            "your_symbol" => "o"
        }),
        self() ! {send, Payload},
        {ok, State#{game_pid => GamePid, game_id => GameIdStr, symbol => "o"}}
    catch
        _:_ ->
            {reply, {text,jsone:encode(#{"error" => "Invalid game ID"})}, State}
    end;

handle_command(#{"type" := "make_move", "game_id" := GameIdBin,
                 "player" := Player, "row" := Row, "col" := Col}, State) ->
    GameId = binary_to_list(GameIdBin),
    try
        GamePid = list_to_pid(GameId),
        case tic_tac_toe_game:make_move(GamePid, Player, #{row => Row, col => Col}) of
            {ok, GameState} ->
                Payload = jsx:encode(#{
                    "type" => "game_state",
                    "status" => GameState#state.status,
                    "board" => GameState#state.board,
                    "current_player" => GameState#state.current_player,
                    "winner" => case GameState#state.winner of undefined -> "undefined"; W -> W end,
                    "game_id" => GameIdBin
                }),
                {reply, {text, Payload}, State};
            {error, Reason} ->
                Error = jsone:encode(#{"error" => Reason}),
                {reply, {text, Error}, State}
        end
    catch
        _:_ ->
            {reply, {text, jsone:encode(#{"error" => "Invalid or expired game_id"})}, State}
    end;


handle_command(_, State) ->
    Error = jsone:encode(#{"error" => "Unknown command"}),
    {reply, {text, Error}, State}.
