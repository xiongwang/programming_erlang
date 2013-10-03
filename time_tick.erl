-module(time_tick).
-compile(export_all).

start(Time, Fun) ->
    register(time_tick, spawn(fun() -> tick(Time, Fun) end)).

stop () -> time_tick ! stop.

tick(Time, Fun) ->
    receive 
        stop ->
            void
    after Time ->
              Fun(),
              tick(Time, Fun)
    end.

