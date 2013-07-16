-module(sqr_sum).
-compile(export_all).

get_sqr(X) -> X * X.

get_sum(1) -> get_sqr(1);
get_sum(N) -> get_sum(N-1) + get_sqr(N).
