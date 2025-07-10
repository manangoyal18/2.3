-module(tic_tac_toe_game).
-behaviour(gen_server).

-export([start/0, make_move/3, get_state/1, reset/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([state_to_map/1]).

-include("tic_tac_toe_game.hrl").

%-record(state, {
%    board,
%    current_player,
%    status,
%    winner
%}).

%% Public API
start() ->
    {ok, _Pid} = gen_server:start(?MODULE, [], []).
   %GameIdStr = erlang:pid_to_list(Pid),
   %ok = game_registry:register_game(GameIdStr, Pid),
   %%{ok, erlang:pid_to_list(Pid)}.
   %{ok, GameIdStr}.

%make_move(GameId, Player, #{<<"row">> := Row, <<"col">> := Col}) ->
%    gen_server:call(list_to_pid(GameId), {make_move, Player, Row, Col}).

make_move(GameId, Player, Move) when is_pid(GameId) ->
    gen_server:call(GameId, {make_move, Player, Move});
make_move(GameIdStr, Player, Move) when is_list(GameIdStr) ->
    case game_registry:get_game_pid(GameIdStr) of
        {ok, Pid} -> gen_server:call(Pid, {make_move, Player, Move});
        Error -> Error
    end.

%get_state(GameId) ->
 %   gen_server:call(list_to_pid(GameId), get_state).

%% Update get_state/1 and other functions to handle both formats
get_state(GameId) when is_pid(GameId) ->
    gen_server:call(GameId, get_state);
get_state(GameIdStr) when is_list(GameIdStr) ->
    case game_registry:get_game_pid(GameIdStr) of
        {ok, Pid} -> gen_server:call(Pid, get_state);
        Error -> Error
    end.

reset(GameId) when is_pid(GameId)->
    gen_server:call(GameId, reset);
reset(GameIdStr) when is_list(GameIdStr) ->
    case game_registry:get_game_pid(GameIdStr) of
        {ok, Pid} -> gen_server:call(Pid, reset);
        Error -> Error
    end.

%% gen_server callbacks
init([]) ->
    InitialBoard = [[<<"">>, <<"">>, <<"">>], 
                   [<<"">>, <<"">>, <<"">>], 
                   [<<"">>, <<"">>, <<"">>]],
    {ok, #state{
        board = InitialBoard,
        current_player = <<"x">>,
        status = <<"ongoing">>,
        winner = undefined
    }}.

handle_call({make_move, Player, #{row := Row, col := Col}}, _From, State) ->
    try
        case valid_move(Player, Row, Col, State) of
            true ->
                NewBoard = update_board(Row, Col, Player, State#state.board),
                NewState = check_game_status(State#state{
                    board = NewBoard,
                    current_player = switch_player(Player)
                }),
                %Response = #{
                %    <<"type">> => <<"game_state">>,
                %    <<"board">> => NewState#state.board,
                %    <<"turn">> => NewState#state.current_player,
                %    <<"status">> => NewState#state.status
                %},
                {reply, {ok, NewState}, NewState};
            {false, Reason} ->
                {reply, {error, Reason}, State}
        end
    catch
        _:_ ->
            {reply, {error, <<"Invalid move">>}, State}
    end;

handle_call(get_state, _From, State) ->
    %Response = #{
    %    <<"type">> => <<"game_state">>,
    %    <<"board">> => State#state.board,
    %    <<"turn">> => State#state.current_player,
    %    <<"status">> => State#state.status
    %},
    {reply, {ok, State}, State};

handle_call(reset, _From, _State) ->
    InitialBoard = [[<<"">>, <<"">>, <<"">>], 
                   [<<"">>, <<"">>, <<"">>], 
                   [<<"">>, <<"">>, <<"">>]],
    NewState = #state{
        board = InitialBoard,
        current_player = <<"x">>,
        status = <<"ongoing">>,
        winner = undefined
    },
   %Response = #{
   %    <<"type">> => <<"game_state">>,
   %    <<"board">> => NewState#state.board,
   %    <<"turn">> => NewState#state.current_player,
   %    <<"status">> => NewState#state.status
   %},
    {reply, {ok, NewState}, NewState};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% Internal functions
valid_move(Player, Row, Col, State) when Row >= 0, Row =< 2, Col >= 0, Col =< 2 ->
    if
        Player =/= State#state.current_player ->
            {false, <<"Not your turn">>};
        State#state.status =/= <<"ongoing">> ->
            {false, <<"Game is over">>};
        true ->
            case lists:nth(Col+1, lists:nth(Row+1, State#state.board)) of
                <<"">> -> true;
                _ -> {false, <<"Cell already taken">>}
            end
    end;
valid_move(_Player, _Row, _Col, _State) ->
    {false, <<"Invalid position">>}.

%%% Helper function to replace element at Index in List
set_list_element(Index, List, Value) ->
    {Front, [_Old | Back]} = lists:split(Index - 1, List),
    Front ++ [Value | Back].

%%% Update the board at given row/col
update_board(Row, Col, Player, Board) ->
    RowList = lists:nth(Row + 1, Board),
    NewRowList = set_list_element(Col + 1, RowList, Player),
    set_list_element(Row + 1, Board, NewRowList).

%%% Switch turns
switch_player(<<"x">>) -> <<"o">>;
switch_player(<<"o">>) -> <<"x">>.

%%% Game status check
check_game_status(State) ->
    case check_winner(State#state.board) of
        {win, Winner} ->
            State#state{status = <<"won">>, winner = Winner};
        draw ->
            State#state{status = <<"draw">>};
        _ ->
            State
    end.

%%% Check for winner
check_winner(Board) ->
    Lines = Board ++ transpose(Board) ++ get_diagonals(Board),
    case lists:filtermap(
        fun(Line) -> case check_line(Line) of
                         {win, A} -> {true, A};
                         _ -> false
                     end
        end, Lines) of
        [Winner | _] ->
            {win, Winner};
        [] ->
            case is_board_full(Board) of
                true -> draw;
                false -> ongoing
            end
    end.

%%% Get diagonals of the board
get_diagonals(Board) ->
    [
        [lists:nth(1, lists:nth(1, Board)), 
         lists:nth(2, lists:nth(2, Board)), 
         lists:nth(3, lists:nth(3, Board))],
        [lists:nth(3, lists:nth(1, Board)), 
         lists:nth(2, lists:nth(2, Board)), 
         lists:nth(1, lists:nth(3, Board))]
    ].

%%% Check if line has a winner
check_line([A, A, A]) when A =/= <<"">> -> {win, A};
check_line(_) -> ongoing.

%%% Check if the board is full
is_board_full(Board) ->
    lists:all(fun(Row) ->
        lists:all(fun(Cell) -> Cell =/= <<"">> end, Row)
    end, Board).

%%% Transpose (rows → columns)
transpose([[A1,A2,A3], [B1,B2,B3], [C1,C2,C3]]) ->
    [[A1,B1,C1], [A2,B2,C2], [A3,B3,C3]].

%%% Convert state to map for JSON response
state_to_map(State) ->
    #{
        board => State#state.board,
        current_player => State#state.current_player,
        status => State#state.status,
        winner => State#state.winner
    }.