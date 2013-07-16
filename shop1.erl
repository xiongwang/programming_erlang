-module(shop1).
-compile(export_all).
-define(FUY, [{oranges,4}, {newspapers,1}, {apples,10}, {pears, 6}, {milk,3}]).

%Fuy = [{oranges, 4}].
demo() ->
    Fuy = ?FUY,
    Fuy.
