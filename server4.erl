-module(server4).
-compile(export_all).

start(Name, Mod) ->
    register(Name, spawn(fun() -> loop(Name, Mod, Mod:init() ) end)).

rpc(Name, Request) ->
    Name ! {self(), Request},
    receive
        {Name, crash} -> exit(rpc);
        {Name, ok, Response} -> Response
    end.

swap_code(Name, Mod) -> rpc(Name, {swap_code, Mod}).

loop(Name, Mod, OldState) ->
    receive
        {From, {swap_code, NewCallbackMod}} ->
            From ! {Name, ok, ack},
            loop(Name, NewCallbackMod, OldState);
        {From, Request} ->
            try Mod:handle(Request, OldState) of
                {Response, NewState} ->
                    From ! {Name, ok, Response},
                    loop(Name, Mod, NewState)
            catch
                _ : Why ->
                    log_the_error(Name, Request, Why),
                    From ! {Name, crash},
                    loop(Name, Mod, OldState)
            end
    end.


log_the_error(Name, Request, Why) ->
    io:format("Server ~p request ~p ~n caused expection ~p~n",
                [Name, Request, Why]).
