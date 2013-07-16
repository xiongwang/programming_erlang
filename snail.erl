-module(snail).
-compile(export_all).

%% The snail go upward 3 miles at daytime and slip down 2 miles every night
%% Input: H miles, height of the well

main(H) ->
    climb(H, 0).

climb(H, M) ->
    if H - 3 > 0 ->
            climb(H-1, M+1);
        H - 3 =:= 0 ->
            M + 1
    end.

