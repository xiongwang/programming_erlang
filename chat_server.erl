-module(chat_server).
-compile(export_all).

-include("chat_server.hrl").

-define(PORT, 4210).

%% Implements
start() ->
    %% 手工建立 用户名-密码 表
    ets:new(userTable, [public, named_table, {keypos, #user.name}] ),
    User1 = #user{id = 1, name="wangxiong", passwd = "123"},
    User2 = #user{id = 2, name="wangzhen", passwd = "123"},
    ets:insert(userTable, User1),
    ets:insert(userTable, User2),

    register(client_controller, spawn(fun() -> deal_with_clients([]) end)),
    {ok, ListenSocket} = gen_tcp:listen(?PORT, [binary, {active, once}, {header, 1} ] ),
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
            %解析得到的Data
            client_controller ! {chat, Data, Socket},  
            handle_client(Socket);  
        {error, closed} ->  
            client_controller ! {disconnect, Socket}  
    end.  
        
%% 验证用户密码, 返回通过true或者失败false
check_user_passwd(Data) ->
    io:format("Now in check_user_passwd~n"),
    [_Opt, Username, Passwd] = Data,
    UserItem = ets:lookup(userTable, Username),
    % 需要从元组中提出Passwd字段!
    [User] = UserItem,
    User#user.passwd =:= Passwd.

%% 维护Clients列表, 并转发消息给所有clients
deal_with_clients(Sockets) ->
    receive
        {connect, Socket} ->
            io:format("Socket connected: ~p~n", [Socket]),
            NewSockets = [Socket | Sockets],
            io:format("Sockets Updated: ~p~n", [NewSockets]);
        {disconnect, Socket} ->
            NewSockets = lists:delete(Socket, Sockets),
            io:format("~p disconnected, Sockets now are ~p~n", [Socket, NewSockets]);
        {chat, Data, Socket} ->
           [Opt | _Rest] = Data,
            case Opt of
                0 ->
                    case check_user_passwd(Data) of
                        true ->
                            Msg = "CHECK PASSED",
                            gen_tcp:send(Socket, Msg);
                        false ->
                            gen_tcp:close(Socket)
                    end;
                1 ->
                    Online_count = length(Sockets),
                    Data = "Online Count is: " ++ integer_to_list(Online_count);

                _AnyOther ->
                    other_reason_in_receive_inside_deal_with_clients
            end,
            
            send_data(Sockets, Data),
            io:format("transfer Data: ~p~n", [Data]),
            NewSockets = Sockets
    end,
    deal_with_clients(NewSockets).

%% server发送消息给所有人
send_data(Sockets, Data) ->
    SendData = (fun(Socket) ->
                io:format("Socket ~p will be sent msg~n", [Socket]),
                io:format("Send ~p~n", [gen_tcp:send(Socket, Data)]) end),
    lists:foreach(SendData, Sockets),
    io:format("finished transferring~n").

%% 获取列表的长度, 已废弃, 使用length/1
%% get_list_length([]) -> 0;
%% get_list_length([ _First | Rest]) -> 1 + get_list_length(Rest).
