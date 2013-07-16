-module(spawn_link_test).
-compile(export_all).

main(Bool, Msg) ->
    A = spawn(fun() -> a() end),
    B = spawn(fun() -> b(A, Bool) end),
    C = spawn(fun() -> c(B, Msg) end),
    %% use sleep/1 to give enough time for printing msgs received - P121
    sleep(1000),
    get_status(b, B),
    get_status(c, C).


a() ->
    process_flag(trap_exit, true),
    wait(a).

b(A, Bool) ->
    process_flag(trap_exit, Bool),
    link(A),
    wait(b).

c(B, Msg) ->
    link(B),
    case Msg of 
        {die, Reason} ->
            exit(Reason);
        %% to test error in dividing with 0 - P121
        {divide, N} ->
            Result = 1/N,
            wait(c);
        normal ->
            true
    end.

sleep(T) ->
    receive
    after T ->
            true
    end.

%% wait/1 to print all msg it received
wait(Prog) ->
    receive 
        Any ->
            io:format("Process ~p got ~p~n", [Prog, Any]),
            wait(Prog)
    end.

get_status(Name, Pid) ->
    case erlang:is_process_alive(Pid) of
        true ->
            io:format("process ~p (~p) is alive~n", [Name, Pid]);
        false ->
            io:format("process ~p (~p) is dead~n", [Name, Pid])
    end.

