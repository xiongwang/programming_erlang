-module(chat_server).
-compile(export_all).
-include("chat_server.hrl").

-define(PORT, 4210).

%% Interfaces

%% Implements
start() ->
    %% 手工建立 用户名-密码 表
    Table = ets:new(myTable, [set]),
    ets:insert(Table, {a, 1}),
    ets:insert(Table, {b, 1}),
    ets:insert(Table, {c, 1}),

    register(client_controller, spawn(fun() -> deal_with_clients([]) end)),
    {ok, ListenSocket} = gen_tcp:listen(?PORT, [binary, {active, false}]),
    do_accept(ListenSocket).

do_accept(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> handle_client(Socket) end),
    client_controller ! {connect, Socket},
    do_accept(ListenSocket).

handle_client(Socket) ->
    io:format("Now in handle_client~n"),
    case gen_tcp:recv(Socket, 0) of 
        {ok, Data} ->
            client_controller ! {chat, Data},  
            handle_client(Socket);  
        {error, closed} ->  
            client_controller ! {disconnect, Socket}  
    end.  
        

%% 维护Clients列表, 并转发消息给所有clients
deal_with_clients(Sockets) ->

    receive
        {connect, Socket} ->
            io:format("Socket connected: ~p~n", [Socket]),
            NewSockets = [Socket | Sockets],
            io:format("Socketed Updated: ~p~n", [NewSockets]);
        {disconnect, Socket} ->
            NewSockets = lists:delete(Socket, Sockets),
            io:format("~p disconnected, Sockets Updated ~p~n", [Socket, NewSockets]);
        {chat, Data} ->
            case (Data =:= term_to_binary("ONLINECOUNT")) of
                true ->
                    Online_count = get_list_length(Sockets),
                    Msg = "Online Count is: " ++ integer_to_list(Online_count),
                    Data = term_to_binary(Msg);
                false ->
                    ""
            end,
            send_data(Sockets, Data),
            io:format("transfer Data: ~p~n", [Data]),
            NewSockets = Sockets
    end,
    deal_with_clients(NewSockets).

%% server发送消息给所有人
send_data(Sockets, Data) ->
    SendData = (fun(Socket) -> gen_tcp:send(Socket, Data) end),
    lists:foreach(SendData, Sockets),
    io:format("finished transfer~n").

%% 获取列表的长度
get_list_length([]) -> 0;
get_list_length([ _First | Rest]) -> 1 + get_list_length(Rest).
