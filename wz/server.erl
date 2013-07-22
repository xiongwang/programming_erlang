-module(server).

-export([startServer/0, wait_connect/2, createTab/0, user_login/2]).
-export([addSocket/3, addLoginTimes/1, addCharTimes/1]).

-record(user, {
      id
    , name
    , passwd
    , login_times
    , char_times
    }).

-record(socket, {username, socket}).

createTab() ->
    ets:new(userTab, [public, ordered_set, named_table, {keypos, #user.name}]),
    U1 = #user{id = 1, name = "wz", passwd = "123", login_times = 0, char_times = 0},
    U2 = #user{id = 2, name = "xy", passwd = "123", login_times = 0, char_times = 0},
    ets:insert(userTab, U1),
    ets:insert(userTab, U2),
    ets:new(socketTab, [public, ordered_set, named_table, {keypos, #socket.username}]).

%建立侦听客户端信息的套接字
startServer() ->
   {ok, ListenSocket} = gen_tcp:listen(1234, [binary, {active, false}, {header, 3}]),
   wait_connect(ListenSocket, 0).

%等待客户端A发出的消息请求
wait_connect(ListenSocket, Count) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    io:format("Ser_Socket: ~w~n", [Socket]),
    spawn(?MODULE, wait_connect, [ListenSocket, Count + 1]),
    get_request(Socket, Count).

%接收客户端A发送的消息
get_request(Socket1, Count) ->
    case gen_tcp:recv(Socket1, 0) of
        {ok, Data} ->
            %解析消息
            [Flag, _, _ | _] = Data,
            case Flag of
                    0 ->             %验证用户名密码，并返回结果
                        checkUserLogin(Data, Socket1),
                        ok;
                    1 ->             %消息传递
                        sendMessage(Data),
                        ok
            end,
            get_request(Socket1, Count);
        {error, closed} ->
            io:format("Socket Closed : ~p~n", [Socket1]),
            ok
    end.

checkUserLogin(Data, Socket1) ->
    [Flag, L1, L2 | Msg] = Data,
    MsgLst = binary_to_list(Msg),
    {User, Rest} = lists:split(L1, MsgLst),
    {Pasw, MsgInfo} = lists:split(L2, Rest),
    %验证用户
    Login = user_login(User, Pasw),
    case Login of
        "pass" ->
            addLoginTimes(User),
             %加入socket表
            addSocket(User, Login, Socket1);
        _ ->
            ok
    end,
    Rdata = [Flag, L1, L2, User, Pasw, Login],
    virtual_client(Socket1, Rdata),
    ok.

sendMessage(Data) ->
    [Flag, L1, L2 | Msg] = Data,
    MsgLst = binary_to_list(Msg),
    {SoucUser, Rest} = lists:split(L1, MsgLst),
    {DestUser, MsgInfo} = lists:split(L2, Rest),
    addCharTimes(SoucUser),
    %获取DestUser的Socket
    DesSocket = getSocketByName(DestUser),
    virtual_client(DesSocket, Data),
    ok.

getSocketByName(UserName) ->
    [#socket{socket = Sock}] = ets:lookup(socketTab, UserName),
    Sock.

addSocket(User, Login, Socket1) when Login == "pass" ->
    %ets:i(),
    ets:insert(socketTab, #socket{username = User, socket = Socket1}).

%用户登录次数加1
addLoginTimes(User) ->
    [UserInfo] = ets:match_object(userTab
          , #user{name = User, _='_'}),
    NewUserInfo = UserInfo#user{login_times = UserInfo#user.login_times + 1},
    ets:insert(userTab, NewUserInfo).

%用户聊天次数加1
addCharTimes(User) ->
    [UserInfo] = ets:match_object(userTab
          , #user{name = User, _='_'}),
    NewUserInfo = UserInfo#user{char_times = UserInfo#user.char_times + 1},
    ets:insert(userTab, NewUserInfo).

user_login(User, Pasw) ->
    UserInfo = ets:match_object(userTab
        , #user{name = User, passwd = Pasw, _='_'}),
    case UserInfo /= [] of
        true ->
            "pass";
        false ->
            "fault"
    end.

%将客户端A的消息发送到客户端B
virtual_client(Socket1, Data) ->
    gen_tcp:send(Socket1, Data).
    %ok = gen_tcp:close(Socket2).