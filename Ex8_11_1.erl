-module(ex8_11_1).
-compile(export_all).

start(atom1, Fun)->
    try register(an_atom, spawn(Fun)) of
        true ->
            io:format("register successfully. ~n")
    catch
        error: _ ->
            io:format("register failed. ~n")
    %% after
       %% io:format("try block's gonna end... ~n")
    end.

main() ->
   start(atom1, fun() -> io:format("atom1 registering...~n") end),
   start(atom1, fun() -> io:format("atom2 registering...~n") end).
