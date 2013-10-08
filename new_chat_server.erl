%=============================================================================
%FileName     : new_chat_server.erl
%Desc         : 聊天室服务端
%Author       : wang xiong
%Email        : xiongwang@live.com
%LastChange   : 2013-10-04 12:03:31
%============================================================================
-module(new_chat_server).
-compile(export_all).

-include("chat_server.hrl").
-define(PORT, 9999).

-record(socket,
        {username,
         socket
        }).

%% @desc: 启动服务器
%% @return: Pid | error
start() ->
    %% 数据初始化
    ets:new(userTable, [
            public,
            named_table,
            {keypos, #user.name}
    ]),

    ets:new(socketTable, [
            public,
            named_table,
            {keypos, #socket.username}
    ]),
    User1 = #user{name = "wangxiong", passwd = "123"},
    User2 = #user{name = "wangzhen", passwd = "123"},
    User3 = #user{name = "doris", passwd = "123"},
    ets:insert(userTable, User1),
    ets:insert(userTable, User2),
    ets:insert(userTable, User3),
    io:format("Server Initializing...~n"),

    %%  监听
    {ok, ListenSocket} = gen_tcp:listen(?PORT, [binary, {active, false}, {header, 3}]),
    io:format("Server Started Listening!~n~n"),
    spawn(fun() -> do_accept(ListenSocket) end).

%% @desc: 接受请求
%% @return: 
do_accept(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> do_accept(ListenSocket) end),
    handle_request(Socket).

%% @desc: 控制流程
%% @return:
handle_request(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            %% 解析消息
            [Flag, _, _ | _] = Data,
            case Flag of
                0 ->    %% 登录
                    io:format("now in check_login~n"),
                    check_login(Data, Socket);
                1 ->    %% 聊天
                    io:format("now in chat send_all~n"),
                    send_all(Data)
                %2 ->    %% 查看在线人数
                %    Len = get_online_cnt(),
                %    Data = [2, Len],
                %    gen_tcp:send(Socket, Data)

            end,
            handle_request(Socket);
        {error, closed} ->
            io:format("Socket Closed: ~p~n", [Socket]),
            ok
    end.

%% @desc: 校验用户名密码
%% @return: bool
check_login(Data, Socket) ->
    [_Flag, L1, L2| Msg ] = Data,
    MsgLst = binary_to_list(Msg),
    {Username, Rest} = lists:split(L1, MsgLst),
    {Passwd, _MsgCont} = lists:split(L2, Rest),
    Result = ets:lookup(userTable, Username),
    case Result of
        [] ->
            false;
        [Rec] when is_record(Rec, user) ->
            case Passwd =:= Rec#user.passwd of
                true ->
                    on_login_succ(Username, Socket),
                    L3 = length(Username),
                    L4 = 0,
                    NewMsg = "pass",
                    NewData = [0, L3, L4, Username, NewMsg],
                    gen_tcp:send(Socket, NewData),
                    true;
                false ->
                    false
            end;
        _ ->
            false
    end.

%% @desc: 校验通过, 更新Socket列表
%% @return: 
on_login_succ(Username, Socket) ->
    [User] = ets:lookup(userTable, Username),
    #user{login_times = LogCnt} = User,
    ets:insert(userTable, User#user{ login_times = LogCnt + 1
                                    ,last_login = erlang:now() }),
    ets:insert(socketTable, #socket{username = Username, socket = Socket}).

%% @desc: 用户离开, 更新Socket列表
%% @return:
on_logout_succ(Username) ->
    ets:delete_object(socketTable, Username).

%% @desc: server转发发送消息给所有人
%% @return: ok.
send_all(Data) ->
    [_Flag, L1, _L2 | _Rest] = Data,
    MsgLst = binary_to_list(Data),
    {Username, _Rest} = lists:split(L1, MsgLst),

    Sockets = ets:tab2list(socketTable),
    F = fun([{Name, Socket}]) ->
                case Name =/= Username of
                    true ->
                        io:format("Send ~p~n", [gen_tcp:send(Socket, Data)]);
                    false ->
                        skip
                end
        end,
    lists:foreach(F, Sockets),
    io:format("finished broadcasting~n"),

    %% 聊天次数+1
    [User] = ets:lookup(userTable, Username),
    #user{chat_times = ChatCnt} = User,
    ets:insert(userTable, User#user{chat_times = ChatCnt + 1}),
    ok.

%% @desc: 查看在线人数
%% @return: int
get_online_cnt() ->
    ets:info(size, userTable).
