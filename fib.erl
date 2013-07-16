-module(fib).
-compile(export_all).

%% regular fib()
fib(0) -> 0;
fib(1) -> 1;
fib(X) ->
    P = fib(X-1) + fib(X-2).
    %%io:format("X = ~w~n", [X]),
    %%io:format("P = ~w~n", [P]),
    %%P.

%% get_fib(Start, End) with regular fib()
get_fib(End, End) -> [fib(End)];
get_fib(Start,End) -> [fib(Start)|get_fib(Start+1 , End)].

%% get_fib(N) with regular fib() 
get_fib2(N) -> fib_list([], N).
fib_list(L, 0) -> L;
fib_list(L, N) -> fib_list([fib(N) | L], N-1).


%% resurive fib() to build lists
get_fib3(0) -> [0];
get_fib3(1) -> [1,0];
get_fib3(N) when N > 1, is_number(N) ->
    %%fib_tail(N, 2, 1, 0, [1,0]),
    lists:reverse(fib_tail(N, 2, 1, 0, [1, 0])).

fib_tail(N, N, R1, R2, L) -> [R1 + R2|L];
fib_tail(N, Index, R1, R2, L) ->
    fib_tail(N, Index + 1, R1+R2, R1, [R1 + R2|L]).


%%tail recursive fib()
fib3_tr(0, Result, _Next) -> Result;
fib3_tr(Iter, Result, Next) when Iter > 0 ->
    fib3_tr(Iter-1, Next, Result + Next).
fib3(N) -> 
    fib3_tr(N, 0, 1).
