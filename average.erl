-module(average).
-compile(export_all).

num([]) -> 0;
num([H|T]) -> 1 + num(T).

sum([]) -> 0;
sum([H|T]) -> H + sum(T).

average([]) -> 0;
average(L) -> sum(L) / num(L).

