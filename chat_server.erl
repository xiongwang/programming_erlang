%=============================================================================
%FileName     : chat_server.erl
%Desc         : 聊天室服务端
%Author       : wang xiong
%Email        : xiongwang@live.com
%LastChange   : 2013-08-06 20:03:31
%============================================================================
-module(chat_server).
-compile(export_all).

-include("chat_server.hrl").

-define(PORT, 9999).

% 手工建立用户名-密码表
initTab() ->
    ets:new(userTable, [public, named_table, {keypos, #user.name}] ),
    User1 = #user{id = 1, name="wangxiong", passwd = "123"},
    User2 = #user{id = 2, name="wangzhen", passwd = "123"},
    ets:insert(userTable, User1),
    ets:insert(userTable, User2).

% 监听
start() ->
    {ok, ListenSocket} = gen_tcp:listen(?PORT, [binary, {active, false}]),
    do_accept(ListenSocket, 0).

% 接受请求
do_accept(ListenSocket, Count) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    io:format("New Socket Came in: ~w~n", [Socket]),
    spawn(?MODULE, do_accept, [ListenSocket, Count + 1]),
    handle_request(Socket, Count).

handle_request(Socket, Count) ->
    io:format("Now in handle_request~n"),
    case gen_tcp:recv(Socket, 0) of 
        {ok, Data} ->
            %解析得到的Data
            [Flag, _Rest] = Data,
            case Flag of
                %登录校验
                0 ->
                    check_user_passwd(Data, Socket);
                %转发消息
                1 ->
                    % 此处如何解决socket和sockets的矛盾??
                    send_all(Socket, Data)
            end,
            handle_request(Socket, Count);
        {error, closed} ->  
            io:format("Socket disconnected: ~p~n", [Socket])      
    end.  
        
% 验证用户密码, 返回通过true或者失败false
check_user_passwd(Data, Socket) ->
    io:format("Now in check_user_passwd!~n"),
    [Flag, LenUsername, LenPasswd | Msg] = Data,
    FullMsg = binary_to_list(Msg),
    {Username, Rest} =lists:split(LenUsername, FullMsg),
    {Passwd, MsgContent} = lists:split(LenPasswd, Rest),
    
    UserObject = ets:match_object(userTable, 
                                  #user{name=Username, passwd=Passwd, _='_'}
                                 ),
    % 判断是否查找到用户
    UserObject =/= [].

% 维护Clients列表, 并转发消息给所有clients
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
           [Opt, _Rest] = Data,
            case Opt of
                0 ->
                    case check_user_passwd(Data, Socket) of
                        true ->
                            io:format("Checking Passed~n"),
                            Msg = "CHECK PASSED",
                            gen_tcp:send(Socket, Msg);
                        false ->
                            io:format("Checking Failed~n"),
                            gen_tcp:close(Socket)
                    end;
                1 ->
                    Online_count = length(Sockets),
                    Data = "Online Count is: " ++ integer_to_list(Online_count);

                _AnyOther ->
                    other_reason_in_receive_inside_deal_with_clients
            end,
            
            send_all(Sockets, Data),
            io:format("transfer Data: ~p~n", [Data]),
            NewSockets = Sockets
    end,
    deal_with_clients(NewSockets).

%% server转发发送消息给所有人
send_all(Sockets, Data) ->
    SendData = (fun(Socket) ->
                        io:format("Socket ~p will be sent msg~n", [Socket])
                        , io:format("Send ~p~n", [gen_tcp:send(Socket, Data)]) 
                end),
    lists:foreach(SendData, Sockets),
    io:format("finished transferring~n").
