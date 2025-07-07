-module(tic_tac_toe_handler).
-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-include_lib("kernel/include/logger.hrl").

init(Req, State) ->
    {cowboy_websocket, Req, State,
    #{idle_timeout => 60000}}.

websocket_init(State0) ->
    ?LOG_INFO("Client connected via WebSocket"),
  %  {ok, State0},
    {ok, Pid} = tic_tac_toe_game:start(),
    GameIdStr = erlang:pid_to_list(Pid),
    GameIdBin = list_to_binary(GameIdStr),
    
    %ok = game_registry:get_game_pid(GameIdStr),

    ok = game_registry:register_game(GameIdStr, Pid),

    %{ok, GameIdStr} = tic_tac_toe_game:start(),
    {ok, InitialState} = tic_tac_toe_game:get_state(Pid),
    
    Payload = jsx:encode(InitialState#{<<"game_id">> => GameIdBin}),

    % Add the game id to the socket state if needed later
    %State1 = State0#{game_id => GameIdStr},
    
    State1 = case is_map(State0) of
        true -> State0#{game_id => GameIdStr};
        false -> #{game_id => GameIdStr}
    end,

    {reply, {text, Payload}, State1}.


%websocket_handle({text, Msg}, State) ->
%    try
%        Parsed = jsone:decode(Msg),
%        handle_command(Parsed, State)
%    catch
%        _:_ ->
%            Error = jsone:encode(#{<<"error">> => <<"Invalid message format">>}),
%            {reply, {text, Error}, State}
%    end;

%websocket_handle({text, Msg}, State) ->
%    try
%        Parsed = jsx:decode(Msg, [return_maps]),
%        io:format("Parsed command: ~p~n", [Parsed]),
%        handle_command(Parsed, State)
%    catch
%        _:_ ->
%            io:format("Failed to decode or handle: ~p~n", [Msg]),
%            Error = jsx:encode(#{<<"error">> => <<"Invalid message format">>}),
%            {reply, {text, Error}, State}
%    end;

websocket_handle({text, Msg}, State) ->
    Parsed = jsone:decode(Msg, [{object_format, map}]),
    io:format("Parsed command: ~p~n", [Parsed]),
    case handle_command(Parsed, State) of
        {reply, Reply, NewState} ->
            {reply, Reply, NewState};
        Other ->
            io:format("Unexpected handle_command result: ~p~n", [Other]),
            {ok, State}
    end;


websocket_handle(_Data, State) ->
    {ok, State}.


websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, _State) ->
    io:format("WebSocket closed: ~p~n", [_Reason]),
    ok.


%% Helper to dispatch based on command type
handle_command(#{<<"type">> := <<"start_game">>}, State) ->
    GameId = tic_tac_toe_game:start(),
    Response = jsone:encode(#{
        <<"type">> => <<"game_started">>,
        <<"game_id">> => list_to_binary(GameId),
        <<"status">> => <<"ongoing">>
    }),
    {reply, {text, Response}, State};

%handle_command(#{<<"type">> := <<"make_move">>,
%                 <<"game_id">> := GameIdBin, 
%                 <<"player">> := Player,
%                 <<"row">> := Row, 
%                 <<"col">> := Col},
%                  State) ->
%
%                try    
%                     GameId = list_to_pid(binary_to_list(GameIdBin)),
%                     case tic_tac_toe_game:make_move(GameId, Player, #{row => Row, col => Col}) of
%                        {ok, GameState} ->
%                            SuccessResponse = jsone:encode(tic_tac_toe_game:state_to_map(GameState)),
%                            {reply, {text, SuccessResponse}, State};
%                        {error, Reason} ->
%                            ErrorResponse = jsone:encode(#{<<"error">> => Reason}),
%                            {reply, {text, ErrorResponse}, State}
%                    end
%                catch
%                     _:_ ->
%                        InvalidPidResponse = jsone:encode(#{<<"error">> => <<"Invalid or expired game_id">>}),
%                        {reply, {text, InvalidPidResponse}, State}
%    end;

handle_command(#{<<"type">> := <<"make_move">>, <<"game_id">> := GameIdBin,
                 <<"player">> := Player, <<"row">> := Row, <<"col">> := Col}, State) ->
    GameIdStr = binary_to_list(GameIdBin),
    case game_registry:get_game_pid(GameIdStr) of
        {ok, GamePid} ->
            case tic_tac_toe_game:make_move(GamePid, Player, #{row => Row, col => Col}) of
                {ok, GameState} ->
                    Response = jsone:encode(tic_tac_toe_game:state_to_map(GameState)),
                    {reply, {text, Response}, State};
                {error, Reason} ->
                    Error = jsone:encode(#{<<"error">> => Reason}),
                    {reply, {text, Error}, State}
            end;
        {error, not_found} ->
            Error = jsone:encode(#{<<"error">> => <<"Game not found">>}),
            {reply, {text, Error}, State}
    end;


handle_command(#{<<"type">> := <<"game_state">>, <<"game_id">> := GameId}, State) ->
    case tic_tac_toe_game:get_state(binary_to_list(GameId)) of
        {ok, GameState} ->
            Response = jsone:encode(tic_tac_toe_game:state_to_map(GameState)),
            {reply, {text, Response}, State};
        {error, Reason} ->
            Error = jsone:encode(#{<<"error">> => Reason}),
            {reply, {text, Error}, State}
    end;

handle_command(#{<<"type">> := <<"reset_game">>, <<"game_id">> := GameId}, State) ->
    case tic_tac_toe_game:reset(binary_to_list(GameId)) of
        {ok, GameState} ->
            Response = jsone:encode(tic_tac_toe_game:state_to_map(GameState)),
            {reply, {text, Response}, State};
        {error, Reason} ->
            Error = jsone:encode(#{<<"error">> => Reason}),
            {reply, {text, Error}, State}
    end;

handle_command(_, State) ->
    Error = jsone:encode(#{<<"error">> => <<"Unknown command">>}),
    {reply, {text, Error}, State}.
