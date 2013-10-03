-module(tut).
-export([print/1]).

print(X)->
io:format("hello world, ~w~n", [X]).