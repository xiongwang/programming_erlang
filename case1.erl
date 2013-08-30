-module(case1).
-compile(export_all).

f(Level) ->
case Level > 40 of
                true when Level =< 60 ->
                    1;
                true when Level =< 80 ->
                    2;
                true when Level > 80 ->
                    3;
                _ ->
                    ok
        end.
