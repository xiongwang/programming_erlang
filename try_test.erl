-module(try_test).
-export([demo1/0, demo2/0, catcher/1, demo3/0]).
-import(try_catch, [generate_expection/1]).

demo1() ->
    [catcher(I) || I <- [1, 2, 3, 4, 5]].

demo2() ->
    [{I, (catch generate_expection(I))} || I <- [1,2,3,4,5]].

demo3() ->
    try generate_expection(5)
    catch error:X ->
        {X, erlang:get_stacktrace()}
    end.

catcher(N) ->
    try generate_expection(N) of 
        Val -> {N, normal, Val}
    catch
        throw:X -> {N, caught, thrown, X};
        exit: X -> {N, caught, exited, X};
        error:X -> {N, caught, error, X}
    end.

