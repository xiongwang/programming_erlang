-module(area_server3).
-compile(export_all).

start() ->
    spawn(fun loop/0).

loop() ->
    receive
        {From, {rectangle, Width, Ht}} ->
            From ! {self(), {rectangle, Width * Ht}},
            loop();
        {From, {circle, R}} ->
            From ! {self(), 3.14159 * R * R},
            loop();
        {From, Other} ->
            From ! {self(), {error, Other}},
            loop
        end.

area(Pid, What) ->
    rpc(Pid, What).

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} ->
            Response
    end.
