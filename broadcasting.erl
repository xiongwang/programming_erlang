-module(broadcasting).
-compile(export_all).

send(IoList) ->
    case inet:ifget("eth0", [broadaddr]) of
        {ok, [{broadaddr, Ip}]} ->
            {ok, Socket} = gen_udp:open(9421, [{broadcast, true}]),
            gen_udp:send(Socket, Ip, 3080, IoList),
            gen_udp:close(Socket);
        _ ->
            io:format("broadcasting not supported.~n")
    end.

listen() ->
    {ok, Socket} = gen_udp:open(3080),
    loop(Socket).

loop(Socket) ->
    receive
        Any ->
            io:format("received: ~p~n", [Any]),
            loop(Socket)
    end.
