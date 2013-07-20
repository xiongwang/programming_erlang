-module(chat_server).
-compile(export_all).
-include("chat_server.hrl").

%% Interfaces
start().
show_online_count().
show_chat_times(Username).
login(Username, Password).
logoff(Username, Password).

%% Implements
start(Port) ->
    register(client_controller, spawn(fun() -> deal_with_clients([]) end)),
    {ok, LinstenSocket} = gen_tcp:listen(Port, [binary, {active, once}]),
    do_accpet(ListenSocket).

do_accept(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    Pid = spawn(fun() -> handle_client(A_Socket) end),
    client_controller ! {connect, A_Socket},
    do_accept(Socket).

handle_client(Socket) ->
    case gen_tcp:recv(Socket, 0).
