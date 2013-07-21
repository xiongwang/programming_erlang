-module(chat_client).
-compile(export_all).

-include("chat_server.hrl").

-define(ADDRESS, localhost).
-define(PORT, 4210).

start() ->
    {ok, Socket} = gen_tcp:connect(?ADDRESS, ?PORT, [binary, {packet, 4}, {active, false}]),
    _Pid = spawn(fun() -> chat(Socket) end),
    receive_msg(Socket).

receive_msg(Socket) ->
    receive 
        {tcp, Socket, Bin} ->
            io:format("Client received: ~p ~n", [binary_to_term(Bin)]),
            receive_msg(Socket)
    end.

chat(Socket) ->
    io:format("~n~n==============Please Choose:~n[1]Send Msg\t[2]Get Online Count\t[3]Quit==============~n"), 
    Choice = io:get_line(""),
    case Choice of
        "1\n" ->
            send_msg(Socket);
        "2\n" ->
            get_online_count(Socket);
        "3\n" ->
            io:format("Good bye!"),
            gen_tcp:shutdown(Socket, read_write);
        _AnyOther ->
            _OK = gen_tcp:shutdown(Socket, read_write),
            io:format("Illegal Input! Program will quit now.~n")
    end.

send_msg(Socket) ->
    Msg = io:get_line("Your Msg Here: "),
    io:format("finished obtainning Msg~n"),
    ok = gen_tcp:send(Socket, term_to_binary(Msg)),
    chat(Socket).

get_online_count(Socket) ->
    io:format("requesting for online count...~n"),
    ok = gen_tcp:send(Socket, term_to_binary("ONLINECOUNT")),
    chat(Socket).
