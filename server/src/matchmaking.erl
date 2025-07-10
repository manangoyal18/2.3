-module(matchmaking).
-behaviour(gen_server).

-export([start_link/0, add_player/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, []}. % queue of PIDs

add_player(Pid) ->
    gen_server:cast(?MODULE, {add_player, Pid}).

handle_cast({add_player, Pid}, []) ->
%    case Queue of
%        [OtherPid | Rest] ->
            % Start game
            {ok, GamePid} = tic_tac_toe_game:start(),
            GameId = tic_tac_toe_handler:make_id(),
            game_registry:register_game(GameId, GamePid),
            game_registry:add_player(GameId, Pid),
%            game_registry:add_player(GameId, OtherPid),

            Pid ! {start_game, GameId, <<"x">>, waiting},

            erlang:send_after(120000, self(), {timeout_waiting_player, GameId}),
            {noreply, [{GameId, Pid}]};  %Keeping track of player2 with game ID


    %       % Assign symbols
    %       OtherPid ! {start_game, GameId, <<"x">>},
    %       Pid ! {start_game, GameId, <<"o">>},

    %       {noreply, Rest};
    %   [] ->
    %       {noreply, [Pid]}
    %nd;

handle_cast({add_player, Pid}, [{GameId, Player1Pid} | Rest]) ->
    % Add Player 2 to existing game
    game_registry:add_player(GameId, Pid),

    % Notify both players
    Player1Pid ! {player2_joined},
    Pid ! {start_game, GameId, <<"o">>, ready},

    {noreply, Rest};

handle_cast(_, State) -> {noreply, State}.

handle_call(_, _From, State) -> {reply, ok, State}.

handle_info({timeout_waiting_player2, GameId}, Queue) ->
    case lists:keytake(GameId, 1, Queue) of
        {value, {GameId, Pid}, Rest} ->
            % Inform Player 1
            Pid ! {game_timeout, GameId},
            {noreply, Rest};
        false ->
            {noreply, Queue}
    end.


terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.
