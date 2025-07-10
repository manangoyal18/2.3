-module(tic_tac_toe_handler).
-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).
-export([handle_command/2]).
-include_lib("kernel/include/logger.hrl").
-include("tic_tac_toe_game.hrl").

init(Req, State) ->
    {cowboy_websocket, Req, State,
    #{idle_timeout => 600000}}.   

websocket_init(State0) ->
    ?LOG_INFO("Client connected via WebSocket"),
    matchmaking:add_player(self()),
    {ok, State0}.
  %  {ok, State0},
  %  {ok, Pid} = tic_tac_toe_game:start(),
   % GameId = make_id(),
    %GameIdStr = erlang:pid_to_list(Pid),
    %GameIdBin = list_to_binary(GameIdStr),    
    %ok = game_registry:get_game_pid(GameIdStr),
%    ok = game_registry:register_game(GameId, Pid),
 %   ok = game_registry:add_player(GameId, self()),

   %%{ok, GameIdStr} = tic_tac_toe_game:start(),
   %{ok, InitialState} = tic_tac_toe_game:get_state(Pid),
   %Response = (tic_tac_toe_game:state_to_map(InitialState))#{
   %    <<"type">> => <<"game_state">>,
   %    <<"game_id">> => list_to_binary(GameId)
   %},    
   %Payload = jsx:encode(Response),
   %% Add the game id to the socket state if needed later
   %%State1 = State0#{game_id => GameIdStr},    
   %State1 = case is_map(State0) of
   %    true -> State0#{game_id => GameId};
   %    false -> #{game_id => GameId}
   %end,
   %{reply, {text, Payload}, State1}.


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
    io:format("Received raw message: ~s~n", [Msg]),
    try
        Parsed = jsone:decode(Msg, [{object_format, map}]),
        io:format("Parsed command: ~p~n", [Parsed]),
        handle_command(Parsed, State)
    catch
        _:Reason ->
            io:format("Failed to decode or handle message: ~p~n", [Reason]),
            Error = jsone:encode(#{<<"error">> => <<"Invalid message format">>}),
            {reply, {text, Error}, State}
    
    %case handle_command(Parsed, State) of
    %    {reply, Reply, NewState} ->
    %        {reply, Reply, NewState};
    %    Other ->
    %        io:format("Unexpected handle_command result: ~p~n", [Other]),
    %        {ok, State}
    end;


websocket_handle(_Data, State) ->
    {ok, State}.

%websocket_info({start_game, GameId, Symbol, ready}, State) ->
%    {ok, GameState} = tic_tac_toe_game:get_state(GameId),
%    Payload = jsx:encode(GameState#{
%        <<"type">> => <<"game_state">>,
%        <<"game_id">> => list_to_binary(GameId),
%        <<"your_symbol">> => Symbol,
%        <<"waiting">> => false
%    }),
%    {[], State#{game_id => GameId, symbol => Symbol}, [{text, Payload}]};
%
%websocket_info({start_game, GameId, Symbol, waiting}, State) ->
%    {ok, GameState} = tic_tac_toe_game:get_state(GameId),
%    Payload = jsx:encode(GameState#{
%        <<"type">> => <<"game_state">>,
%        <<"game_id">> => list_to_binary(GameId),
%        <<"your_symbol">> => Symbol,
%        <<"current_player">> => <<"x">>,
%        <<"waiting">> => true
%    }),
%    {[], State#{game_id => GameId, symbol => Symbol}, [{text, Payload}]};

websocket_info({start_game, GameId, Symbol, waiting}, State) ->
    send_full_game_state(GameId, Symbol, self()),
    {[], State#{game_id => GameId, symbol => Symbol}, []};

websocket_info({start_game, GameId, Symbol, ready}, State) ->
    send_full_game_state(GameId, Symbol, self()),
    {[], State#{game_id => GameId, symbol => Symbol}, []};


websocket_info({player2_joined}, State) ->
    Msg = jsx:encode(#{type => <<"player2_joined">>, message => <<"Second player connected. You can continue.">>}),
    {[], State, [{text, Msg}]};

websocket_info({game_timeout, GameId}, State) ->
    Msg = jsx:encode(#{
        type => <<"game_timeout">>,
        <<"game_id">> => list_to_binary(GameId),
        message => <<"Game cancelled. No second player joined.">>
    }),
    {[], State, [{text, Msg}]};


websocket_info({send, Payload}, State) ->
    io:format("Sending to client: ~s~n", [Payload]),
    {reply, {text, Payload}, State};

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

handle_command(#{<<"type">> := <<"make_move">>, 
                 <<"game_id">> := GameIdBin,
                 <<"player">> := Player,
                 <<"row">> := Row,
                 <<"col">> := Col}, State) ->

    GameId = binary_to_list(GameIdBin),

    case game_registry:get_game_pid(GameId) of
        {ok, GamePid} ->
            case tic_tac_toe_game:make_move(GamePid, Player, #{row => Row, col => Col}) of
                {ok, GameState} ->
                    ok = broadcast_game_state(GameId, GameState),
                    {ok, State};
                {error, Reason} ->
                    Error = jsone:encode(#{<<"error">> => Reason}),
                    {reply, {text, Error}, State}
            end;
        {error, not_found} ->
            Error = jsone:encode(#{<<"error">> => <<"Game not found">>}),
            {reply, {text, Error}, State}
    end;


handle_command(#{<<"type">> := <<"game_state">>, <<"game_id">> := GameIdBin}, State) ->
    GameId = binary_to_list(GameIdBin),
    case tic_tac_toe_game:get_state(GameId) of
        {ok, GameState} ->
            Response = (tic_tac_toe_game:state_to_map(GameState))#{
                <<"type">> => <<"game_state">>,
                <<"game_id">> => GameIdBin
            },
            {reply, {text, jsone:encode(Response)}, State};
        {error, Reason} ->
            Error = jsone:encode(#{<<"error">> => Reason}),
            {reply, {text, Error}, State}
    end;

handle_command(#{<<"type">> := <<"reset_game">>, <<"game_id">> := GameIdBin}, State) ->
    GameId = binary_to_list(GameIdBin),
    case tic_tac_toe_game:reset(GameId) of
        {ok, GameState} ->
            Response = (tic_tac_toe_game:state_to_map(GameState))#{
                <<"type">> => <<"game_state">>,
                <<"game_id">> => GameIdBin
            },
            {reply, {text, jsone:encode(Response)}, State};
        {error, Reason} ->
            Error = jsone:encode(#{<<"error">> => Reason}),
            {reply, {text, Error}, State}
    end;


handle_command(_, State) ->
    Error = jsone:encode(#{<<"error">> => <<"Unknown command">>}),
    {reply, {text, Error}, State}.

%% Generate a fallback unique ID (no deps)
make_id() ->
    Now = erlang:monotonic_time(),
    Unique = erlang:unique_integer([monotonic, positive]),
    integer_to_list(Now) ++ "-" ++ integer_to_list(Unique).

broadcast_game_state(GameId, GameState) ->
    Response = (tic_tac_toe_game:state_to_map(GameState))#{
        <<"type">> => <<"game_state">>,
        <<"game_id">> => list_to_binary(GameId)
    },
    Payload = jsone:encode(Response),
    Pids = game_registry:get_players(GameId),
    [Pid ! {send, Payload} || Pid <- Pids],
    ok.

send_full_game_state(GameId, Symbol, SocketPid) ->
    case tic_tac_toe_game:get_state(GameId) of
        {ok, State} ->
            Payload = jsx:encode(#{
                <<"type">> => <<"game_state">>,
                <<"status">> => State#state.status,
                <<"board">> => State#state.board,
                <<"current_player">> => State#state.current_player,
                <<"winner">> => case State#state.winner of undefined -> <<"undefined">>; W -> W end,
                <<"game_id">> => list_to_binary(GameId),
                <<"your_symbol">> => Symbol
            }),
            SocketPid ! {send, Payload},
            ok;
        Error ->
            Error
    end.

