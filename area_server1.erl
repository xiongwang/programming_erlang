-module(area_server2).
-compile(export_all).

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} ->
            Response
    end.

loop() ->
    receive
        {From, {rectangle, Width, Ht}} ->
            %% io:format("Area of rectangle is ~p~n", [Width * Ht]),
            From ! {self(), Width * Ht},
            loop();
        {From, {circle, R}} ->
            %% io:format("Area of circle is ~p~n", [3.14159 * R * R]),
            From ! {self(), 3.14159 * R * R},
            loop();
        {From, Other} ->
            %% io:format("Unable to calculate area of ~p~n", [Other]),
            From ! {self(),{error, Other}},
            loop()
    end.
