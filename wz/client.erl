-module(client).
-export([create_userTab/0, user_login/2, connect/0, said/2, get_request/1]).

create_userTab() ->
     %新建立一个ets.
    ets:new(userInfo, [named_table, public]).

%用户登录
user_login(UserName, Password) ->
    %尝试关闭该用户之前的登录进程
    user_logout(UserName),
    %首先进行连接
    UserSocket = connect(),
    %构建登录消息发送到客户端  [0,L1,L2, U, P];
    L1 = length(UserName),
    L2 = length(Password),
    Data = [0, L1, L2, UserName, Password],
    sendMsg(UserSocket, Data).

user_logout(UserName) ->
    UserInfo = ets:lookup(userInfo, UserName),
    case UserInfo /= [] of
        true ->
            [ {_, _, Socket}] = UserInfo,
            io:format("Socket Close : ~p~n", [Socket]),
            gen_tcp:close(Socket);
        false ->
            ok
    end.

connect() ->
     {ok, Socket1} = gen_tcp:connect({127,0,0,1}, 1234, [binary, {active,false}, {header, 3}]),
     spawn(?MODULE, get_request, [Socket1]),
     Socket1.

said(DesUser, Msg) ->
    UserName = ets:first(userInfo),
    [ {_, _, Socket}] = ets:lookup(userInfo, UserName),
    L1 = length(UserName),
    L2 = length(DesUser),
    Data = [1, L1, L2, UserName, DesUser, Msg],
    sendMsg(Socket, Data).

%Flag: 0表示发送连接确认，将用户名发给服务端；1~n表示接收用户的id
%发送消息给服务器
sendMsg(Socket1, Data) ->
    %send(Socket1, RealData),
    gen_tcp:send(Socket1, Data).
    %ok = gen_tcp:close(Socket1).

%接收
get_request(Socket1) ->
    case gen_tcp:recv(Socket1, 0) of
         {ok, Data} ->
         %解析消息
            [Flag, L1, L2 | Msg] = Data,
            MsgLst = binary_to_list(Msg),
            case Flag of
                    0 ->             %返回登录
                        {User, Rest} = lists:split(L1, MsgLst),
                        {Pasw, MsgInfo} = lists:split(L2, Rest),
                        case MsgInfo == "pass" of
                            true ->
                                %插入一个用户信息
                                ets:insert(userInfo, {User, Pasw, Socket1}),
                                io:format("Login Pass~n");
                            false ->
                                io:format("Login Fault~n")
                                %haut()
                        end;
                    1 ->             %消息传递
                        {SoucUser, Rest} = lists:split(L1, MsgLst),
                        {DestUser, MsgInfo} = lists:split(L2, Rest),
                        io:format("~p said : ~p~n", [SoucUser, MsgInfo]),
                        ok
            end,
            get_request(Socket1);
        {error, closed} ->
            ok
    end.
