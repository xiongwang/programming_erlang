-module(chat_client).
-compile(export_all).

-include("chat_server.hrl").

-define(ADDRESS, localhost).
-define(PORT, 4210).

login(Username, Passwd) ->
    {ok, Socket} = gen_tcp:connect(?ADDRESS, ?PORT, [binary, {packet, 0}, {active, false}, {header, 1}]),
    Login_Request = [0, Username, Passwd],
    ok = gen_tcp:send(Socket, Login_Request),
    io:format("finished sending login_request~n"),

    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            case ("CHECK PASSED" =:= Data) of
                true ->
                    start(Socket);
                false ->
                     io:format("login failed~n")
             end;
        {error, _closed} ->
            io:format("error encountered when log in~n")
    end.

start(Socket) ->
    %% {ok, Socket} = gen_tcp:connect(?ADDRESS, ?PORT, [binary, {packet, 4}, {active, once}]),
    _Pid = spawn(fun() -> chat(Socket) end),
    receive_msg(Socket).

receive_msg(Socket) ->
    receive 
        {tcp, Socket, Bin} ->
            io:format("~nClient received: ~p~n", [Bin]),
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

get_online_count(Socket) ->
    io:format("requesting for online count...~n"),
    New_Msg = [1, "ONLINECOUNT"],
    ok = gen_tcp:send(Socket, New_Msg),
    chat(Socket).

send_msg(Socket) ->
    Msg = io:get_line("Your Msg Here: "),
    New_Msg = [2, Msg],
    io:format("finished obtainning Msg~n"),
    ok = gen_tcp:send(Socket, New_Msg),
    chat(Socket).
