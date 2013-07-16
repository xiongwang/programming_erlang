-module(try_catch).
-export([generate_expection/1]).

generate_expection(1) -> a;
generate_expection(2) -> throw(a);
generate_expection(3) -> exit(a);
generate_expection(4) -> {'EXIT', a};
generate_expection(5) -> erlang:error(a).
