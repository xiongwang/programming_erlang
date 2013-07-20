-module(my_name_server).
-behaviour(gen_server).
-compile(export_all).

-define(my_name_server, ?MODULE).

%% APIs
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

set_name(ID, Name) -> gen_server:call(?MODULE, {add, ID, Name}).
get_name(ID) -> gen_server:call(?MODULE, {lookup, ID}).
remove_name(ID) -> gen_server:call(?MODULE, {remove, ID}).
display_all_names() -> gen_server:call(?MODULE, {display_all_names}).
remove_all_names() -> gen_server:call(?MODULE, {remove_all_names}).

%% callback routines
init([]) -> {ok, local}.

handle_call({add, ID, Name}, _From, Tab) ->
    {reply, do_set_name(ID, Name), Tab+1};

handle_call({lookup, ID}, _From, Tab) ->
    {reply, do_get_name(ID), Tab+1};

handle_call({remove, ID}, _From, Tab) ->
    {reply, do_delete_name(ID), Tab+1};

handle_call({display_all_names}, _From, Tab) ->
    {reply, do_display_all(), Tab};

handle_call({remove_all_names}, _From, Tab) ->
    {reply, do_remove_all(), Tab};

handle_call(stop, _From, Tab) ->
    {stop, normal, stopped, Tab}.

%% default implement
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% private implement
do_set_name(ID, Name) ->
    erlang:put(ID, Name).

do_get_name(ID) ->
    erlang:get(ID).

do_delete_name(ID) ->
    erlang:erase(ID).

do_display_all() ->
    erlang:get().

do_remove_all() ->
    erlang:erase().

