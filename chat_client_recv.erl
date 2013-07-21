-module(chat_client_recv).
-compile(export_all).

-include("chat_server.hrl").

-define(ADDRESS, localhost).
-define(PORT, 4210).

start() ->
    {ok, Socket} = gen_tcp:connect(?ADDRESS, ?PORT, [binary, {packet, 4}]),
    chat(Socket).


chat(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            io:format("now in recv!~n"),
            Msg = binary_to_term(Bin),
            io:format("Received Msg: ~p~n", [Msg]),
            chat(Socket)
    end.
