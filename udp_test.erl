-module(udp_test).
-compile(export_all).

start_server() ->
    spawn(fun() -> server(3000) end).

%% here is the udp server
server(Port) ->
    {ok, Socket} = gen_udp:open(Port, [binary]),
    io:format("Server opened Socket is ~p~n",[Socket]),
    loop(Socket).

loop(Socket) ->
    receive
            {udp, Socket, Host, Port, Bin} = Msg ->
                io:format("Server received: ~p~n", [Msg]),
                {Ref, N} = binary_to_term(Bin),
                Fac = fac(N),
                gen_udp:send(Socket, Host, Port, term_to_binary({Ref, Fac})),
                loop(Socket);
            _AnyOther ->
                io:format("Packet format wrong!~n"),
                loop(Socket)
        end.
% the power calculator
fac(0) -> 1;
fac(N) -> N * fac(N-1).

%% the client
client(Request) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    io:format("client open socket: ~p~n", [Socket]),
    Ref = make_ref(), %% to make an unique ID
    ok = gen_udp:send(Socket
                , "localhost"
                , 3000
                , term_to_binary({Ref, Request})),
    wait_ref(Socket, Ref).


%% Ref identifier
wait_ref(Socket, Ref) ->
    receive
        {udp, Socket, _, _, Bin } ->
            case binary_to_term(Bin) of
                {Ref, Val} ->
                    io:format("Ref identification passed.~n"),
                    io:format("the result is: ~p~n", [Val]);
                {_OtherRef, _} ->
                    io:format("Input Error~n"),
                    wait_ref(Socket, Ref)
            end
    after 2000 ->
            io:format("timeout~n")
    end,
    
    io:format("~~~~~~~~~~~~~~ finished printing Value~n"),
    gen_udp:close(Socket).
