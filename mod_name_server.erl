-module(mod_name_server).
-compile(export_all).

start_me_up(MM, _ArgsC, _ArgS) ->
    loop(MM).

loop(MM) ->
    receive 
        {chan, MM, {store, K, V}} ->
            kvs:store(K, V),
            loop(MM);
        {chan, MM, {lookup, K}} ->
            kvs:lookup(K),
            loop(MM);
        {chan_closed, MM} ->
            true
    end.
