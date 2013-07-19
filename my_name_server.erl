-module(my_name_server).
-behaviour(gen_server).
-compile(export_all).

-define(my_name_server, ?MODULE).

%% APIs
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

set_name(ID, Name) -> gen_server:call(?MODULE, {add, ID, Name}).
get_name(ID) -> gen_server:call(?MODULE, {lookup, ID}).
get_stat() -> gen_server:call(?MOD, {get_stat}).

%% callback routines
init([]) -> {ok}.

%% deal with set_name/2
handle_call({add, ID, Name}, _From, Tab) ->
    Reply = case put(ID, Name) of
        Name -> {record_exists};
        AnyOther ->
            AnyOther
    end,
    {reply, Reply, Tab+1};

%% deal with get_name/1
handle_call({lookup, ID}, _From, Tab) ->
    Reply = case get(ID) of
        [Str] ->
            {the_name_is, [Str]};
        _AnyOther ->
            {record_doesnt_exists}
    end,
    {reply, Reply, Tab+1};

handle_call(stop, _From, Tab) ->
    {stop, normal, stopped, Tab}.

%% deal with get_stat/0
%%handle_call({get_stat}, _From, State) ->
  %%  io:format("Set ~p times ~n", [WState]),
    %%io:format("Get ~p times ~n", [RState]).


handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}. 
