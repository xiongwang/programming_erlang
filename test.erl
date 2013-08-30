-module(test).
-compile(export_all).

do_check_is_nearby([]) ->
    ok;
do_check_is_nearby(MemberLst) ->
    [Head | Rest] = MemberLst,
    F= fun(Mate) -> 
        io:format("[~p] vs [~p]~n", [Head, Mate])
    end,
    lists:foreach(F, Rest),
    do_check_is_nearby(Rest).

