-module(lib_misc).
-export([qsort/1, pythag/1, perms/1]).

qsort([]) -> [];
qsort([Pivot|T]) ->
    qsort([X || X<- T, X < Pivot])
    ++ [Pivot] ++
    qsort([X || X<- T, X >= Pivot]).

pythag(N) ->
    [ {A, B, C} ||
        A <- lists:seq(1, N),
        B <- lists:seq(1, N),
        C <- lists:seq(1, N),
        A+B+B =< N,
        A*A + B*B =:= C*C
    ].
    
perms([]) -> [[]];
perms(L) -> [[H|T] || H <- L, T <- perms(L--[H])].
