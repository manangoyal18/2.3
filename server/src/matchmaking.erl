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

handle_cast({add_player, Pid}, Queue) ->
    case Queue of
        [OtherPid | Rest] ->
            % Start game
            {ok, GamePid} = tic_tac_toe_game:start(),
            GameId = tic_tac_toe_handler:make_id(),
            game_registry:register_game(GameId, GamePid),
            game_registry:add_player(GameId, Pid),
            game_registry:add_player(GameId, OtherPid),

            % Assign symbols
            Pid ! {start_game, GameId, <<"x">>},
            OtherPid ! {start_game, GameId, <<"o">>},

            {noreply, Rest};
        [] ->
            {noreply, [Pid]}
    end;
handle_cast(_, State) -> {noreply, State}.

handle_call(_, _From, State) -> {reply, ok, State}.
handle_info(_, State) -> {noreply, State}.
terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.
