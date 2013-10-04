%=============================================================================
%FileName     : new_chat_client.erl
%Desc         : 聊天室客户端
%Author       : wang xiong
%Email        : xiongwang@live.com
%LastChange   : 2013-08-06 20:03:31
%============================================================================
-module(new_chat_client).
-compile(export_all).

-include("chat_server.hrl").

-define(ADDRESS, localhost).
-define(PORT, 9999).

user_login(Username, Passwd) ->
    user_logout(Username),
    {ok, ConnectSocket} = gen_tcp:connect(?ADDRESS, ?PORT, [binary, {active, false}, {header, 3}]),
    spawn(?MODULE, do_request, [ConnectSocket]),
    
    %% 组装请求
    L1 = length(Username),
    L2 = length(Passwd),
    Data = [0, L1, L2, Username, Passwd],
    gen_tcp:send(ConnectSocket, Data).

user_logout(Username) ->
    User = ets:lookup(socketTable, Username),
    case User =/= [] of
        true ->
            [{_Username, Socket}] = User,
            io:format("Socket Closed: ~p~n", [Socket]),
            gen_tcp:close(Socket);
        false ->
            skip
    end.
chat(MsgContent) ->
    Socket = get(socket),
    Username = get(username),
    chat(Socket, Username, MsgContent).

chat(Socket, Username, MsgContent) ->
    L1 = length(Username),
    L2 = 0,
    Data = [1, L1, L2, Username, MsgContent],
    gen_tcp:send(Socket, Data).

do_request(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            [Flag, L1, L2 | Msg] =Data,
            MsgLst = binary_to_list(Msg),
            case Flag of
                0 ->
                    {Username, Rest} = lists:split(L1, MsgLst),
                    {_Passwd, MsgContent} = lists:split(L2, Rest),
                    case MsgContent =:= "pass" of
                        true ->
                            put(username, Username),
                            put(socket, Socket),
                            io:format("Login Successfully.~n");
                        false ->
                            io:format("Login Failed~n")
                    end;
                1 ->
                    {SrcUser, Rest} = lists:split(L1, MsgLst),
                    io:format("~p Says: ~p~n", [SrcUser, Rest])
            end,
            do_request(Socket);
        {error, closed} ->
            ok
    end.
            
