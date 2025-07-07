-module(tic_tac_toe_router).

-export([routes/0]).

routes() -> [
    {"/websocket", tic_tac_toe_handler, []}
].
