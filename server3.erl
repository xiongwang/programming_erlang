-module(server3).
-compile(export_all).

start(Name, Mod) ->
    register(Name,
            spawn(fun() -> loop(Name, Mod, Mod:init()) end)).

swap_code(Name, Mod) -> rpc(Name, {swap_code, Mod}).

rpc(Name, Request) ->
    Name ! {self(), Request},
    receive
        {Name, crash} -> exit(rpc);
        {Name, Response} -> Response
    end.

loop(Name, Mod, OldState) ->
    receive
        {From, {swap_code, NewCallBackMod}} ->
            From ! {Name, ack},
            loop(Name, NewCallBackMod, OldState);
        {From, Request} ->
            {Response, NewState} = Mod:handle(Request, OldState),
            From ! {Name, Response},
            loop(Name, Mod, NewState)
    end.

log_the_error(Name, Request, Why) ->
    io:format("Server ~p request ~p ~n" "caused expection ~p~n"
        , [Name, Request, Why]).
