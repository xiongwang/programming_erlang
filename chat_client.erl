-module(chat_client).
-compile(export_all).

start(Address, Port) ->
    {ok, Socket} = gen_tcp:connect(IP, Port, [binary, {packet, 4}]),
%%    chat(Socket, Operation, Msg).
    chat(Socket).

chat(Socket) ->
    Msg = io:get_line("your msg here:~n").
    ok =  gen_tcp:send(Socket, term_to_binary(Msg)),
    receive
        {tcp, Socket, Bin} ->
            io:format("now in receive~nMsg received:~w", binary_to_term(Bin)),
%%            gen_tcp:close(Socket)
    end.

    chat(Socket).

