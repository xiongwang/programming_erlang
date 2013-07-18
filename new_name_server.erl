-module(new_name_server).
-compile(export_all).
-import(server4, [rpc/2]).

%% client routines


%% interfaces
all_names() -> rpc(name_server, allNames).
delete(Name) -> rpc(name_server, {delete, Name}).
add(Name, Place) -> rpc(name_server, {add, Name, Place}).
whereis(Name)    -> rpc(name_server, {whereis, Name}).

%% callback routines
init() -> dict:new().

handle({add, Name, Place}, Dict) ->
    {ok, dict:store(Name, Place, Dict)};
handle({whereis, Name}, Dict) ->
    {dict:find(Name, Dict), Dict};
handle({delete, Name}, Dict) ->
    {dict:erase(Name, Dict)};
handle(allNames, Dict) ->
    {dict:fetch_keys(Dict), Dict}.

