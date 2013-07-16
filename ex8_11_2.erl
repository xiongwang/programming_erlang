-module(ex8_11_2).
-compile(export_all).
-define(M, 1000). %% each Msg will be sent M times
-define(N, 200000). %% there will be N processes

main() ->
    doit(?N, ?M).

doit(N, M) ->
    statistics(runtime),
    statistics(wall_clock),

    %% create processes here
    L = create_processes(N, []),
    %% send msgs here
    send_msg(L, M),

    {_, Time_of_Machine} = statistics(runtime),
    {_, Time_of_CPU} = statistics(wall_clock),

    io:format("Time cost: ~p(~p) ~n", [Time_of_CPU, Time_of_Machine]).

%% implement of create_processes/2
create_processes(0, L) -> L;
create_processes(N, L) ->
    Pid = spawn(fun loop/0), %% loop/0, the msg receiver
    create_processes(N-1, [Pid | L]).

%% implement of send_msgs/2
send_msg(_, 0) -> void;
send_msg(L, M) ->
    [ Target ! [hello] || Target <- L],
    send_msg(L, M-1).

%% implement of msg receiver loop/0
loop() ->
    receive
        _Any ->
            loop()
    end.
