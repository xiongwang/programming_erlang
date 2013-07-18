-module(ets_test).
-compile(export_all).

start() ->
    lists:foreach(fun test_ets/1, [set, ordered_set, bag, duplicate_bag]),
    io:format("~n"),
    do_lookup(),
    io:format("~n"),
    do_delete().


TableID = test_ets(Mode) ->
    TableId = ets:new(test, [Mode]),
    ets:insert(TableId, {a,1}),
    ets:insert(TableId, {b,2}),
    ets:insert(TableId, {a,1}),
    ets:insert(TableId, {a,4}),
    List = ets:tab2list(TableId),
    io:format("~-13w => ~p~n", [Mode, List]),
    %% ets:delete(TableId).
    io:foramt("~p~n", TableId).

do_lookup(Ta
