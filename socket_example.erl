-module(socket_example).
-compile(export_all).

%% here are the functions to get web content
get_url() ->
    get_url("www.renren.com").
get_url(Host) ->
    {ok, Socket} = gen_tcp:connect(Host, 80, [binary, {packet, 0}]),
    ok = gen_tcp:send(Socket, "GET / HTTP/1.0\r\n\r\n"),
    receive_data(Socket, []).

receive_data(Socket, SoFar) ->
    receive
        {tcp, Socket, Bin} ->
            receive_data(Socket, [Bin | SoFar]);
        {tcp_closed, Socket} ->
            list_to_binary(lists:reverse(SoFar))
    end.

%% here is the normal-only-run-once server
start_server() ->
    {ok, Listen} = gen_tcp:listen(
            9421
            , [binary
            , {packet, 4}
            , {reuseaddr, true}
            , {active, true}]), %% passive socket 
    {ok, Socket} = gen_tcp:accept(Listen),
    gen_tcp:close(Listen),
    loop(Socket).

loop(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            io:format("Server Received Binary = ~p~n", [Bin]),
            Str = binary_to_term(Bin),
            io:format("Server Unpacked ~p~n", [Str]),
            Reply = string2value(Str),
            io:format("Server replying: ~p~n", [Reply]),
            gen_tcp:send(Socket, term_to_binary(Reply)),
            loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Server socket closed~n")
    end.


%% string2value/1, quoted from media.pragprog.com/titles/jaerlang/code/lib.misc.erl
%% copyleft.
string2value(Str) ->
    {ok, Tokens, _} = erl_scan:string(Str ++ "."),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    Bindings = erl_eval:new_bindings(),
    {value, Value, _} = erl_eval:exprs(Exprs, Bindings),
    Value.


%% client for testing
client_eval(Str) ->
    {ok, Socket} = gen_tcp:connect("localhost", 9421, [binary, {packet, 4}]),
    ok = gen_tcp:send(Socket, term_to_binary(Str)),
    receive
        {tcp, Socket, Bin} ->
            io:format("Client received binary = ~p~n", [Bin]),
            Val = binary_to_term(Bin),
            io:format("Client result = ~p~n", [Val]),
            gen_tcp:close(Socket)
    end.

%% start_server PRO version - start_seq_server
start_seq_server() ->
    {ok, Listen} = gen_tcp:listen(
            9421
            , [binary
            , {packet, 4}
            , {reuseaddr, true}
            , {active, true}]),
    loop_seq_server(Listen).

loop_seq_server(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    loop(Socket),
    io:format("entering the loop_seq_server() again~n"),
    loop_seq_server(Listen).

%% here is the parallel server
start_parallel_server() ->
     {ok, Listen} = gen_tcp:listen(
            9421
            , [binary
            , {packet, 4}
            , {reuseaddr, true}
            , {active, true}]),
    spawn(fun() -> par_connect(Listen) end).

par_connect(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> par_connect(Listen) end),
    loop(Socket).

%% here are functions deal with errors
error_test() ->
    spawn(fun() -> error_test_server() end),
    
    lib_misc:sleep(3000),
    {ok, Socket} = gen_tcp:connect("localhost", 9421, [binary, {packet, 2}]),
    io:format("connected to: ~p~n", [Socket]),
    gen_tcp:send(Socket, <<"123">>),
    receive
        Any ->
            io:format("Any = ~p~n", [Any])
    end.

error_test_server() ->
    {ok, Listen} = gen_tcp:listen(9421, [binary, {packet, 2}]),
    {ok, Socket} = gen_tcp:accept(Listen),
    error_test_server_loop(Socket).

error_test_server_loop(Socket) ->
    receive
        {tcp, Socket, Data} ->
            io:format("received: ~p~n", [Data]),
            _ = atom_to_list(Data),
            error_test_server_loop(Socket)
    end.
