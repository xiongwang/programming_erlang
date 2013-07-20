-module(chat_server).
-compile(export_all).
-include("chat_server.hrl").

%% Interfaces

%% Implements
start(Port) ->
    register(client_controller, spawn(fun() -> deal_with_clients([]) end)),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, once}]),
    do_accept(ListenSocket).

do_accept(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    _Pid = spawn(fun() -> handle_client(Socket) end),
    client_controller ! {connect, Socket},
    do_accept(ListenSocket).

handle_client(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            client_controller ! {data, Data},
            handle_client(Socket);
        {error, closed} ->
            client_controller ! {disconnect, Socket}
    end.

%% server发送消息给所有人
send_data(Sockets, Data) ->
    SendData = (fun(Socket) -> gen_tcp:send(Socket, Data) end),
    lists:foreach(SendData, Sockets).

%% 维护Clients列表, 并转发消息给所有clients
deal_with_clients(Sockets) ->
    receive
        {connect, Socket} ->
            io:format("~nSocket connected: ~p~n", [Socket]),
            NewSockets = [Socket | Sockets];
        {disconnect, Socket} ->
            io:format("~nSocket disconnected: ~p~n", [Socket]),
            NewSockets = lists:delete(Socket, Sockets);
        {chat, Data} ->
            send_data(Sockets, Data),
            NewSockets = Sockets
    end,
    deal_with_clients(NewSockets).
