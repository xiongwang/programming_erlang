-module(my_name_server).
-behaviour(gen_server).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).
-compile(export_all).

-define(my_name_server, ?MODULE).

-record(counter, {
         set_times = 0 
        ,get_times = 0
        ,delete_times = 0
    }).

%% APIs
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

set_name(ID, Name) -> gen_server:call(?MODULE, {add, ID, Name}).
get_name(ID) -> gen_server:call(?MODULE, {lookup, ID}).
remove_name(ID) -> gen_server:call(?MODULE, {remove, ID}).
display_all_names() -> gen_server:call(?MODULE, {display_all_names}).
remove_all_names() -> gen_server:call(?MODULE, {remove_all_names}).
get_stat() -> gen_server:call(?MODULE, {get_stat}).

%% callback routines
init([]) -> {ok, #counter{} }.

handle_call({add, ID, Name}, _From, State) ->
    {reply, do_set_name(ID, Name), State#counter{set_times = State#counter.set_times +1} };

handle_call({lookup, ID}, _From, State) ->
    {reply, do_get_name(ID), State#counter{get_times = State#counter.get_times +1}};

handle_call({remove, ID}, _From, State) ->
    {reply, do_remove_name(ID), State#counter{delete_times = State#counter.delete_times +1}};

handle_call({display_all_names}, _From, State) ->
    {reply, do_display_all(), State#counter{get_times = State#counter.get_times +1} };

handle_call({remove_all_names}, _From, State) ->
    {reply, do_remove_all(), State#counter{delete_times = State#counter.delete_times +1}};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({get_stat}, _From, State) ->
    {reply, do_show_stat(State), State}.

%% default implement
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% private implement
do_set_name(ID, Name) ->
    io:format("now in do_set_name~n"),
    erlang:put(ID, Name),
    ok.

do_get_name(ID) ->
    io:format("now in do_get_name~n"), 
    erlang:get(ID).

do_remove_name(ID) ->
    io:format("now in do_remove_name~n"),
    erlang:erase(ID).

do_display_all() ->
    io:format("now in display_all~n"),
    erlang:get().

do_remove_all() ->
    io:format("now in remove_all~n"),
    erlang:erase().

do_show_stat(#counter{set_times = S, get_times = G, delete_times = D} = _State) ->
    io:format("now in do_remove_all~n"),
    io:format("Set_times = ~p, Get_times = ~p, Delete_times = ~p~n", [S, G, D]).
