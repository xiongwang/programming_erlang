-module(name).
-export([start_link/0, stop/0 ]).
-export([add_name/1, del_name/1, get_name_byid/1]).
-export([get_writenum/0, get_readnum/0]).
-export([init/1, terminate/2, handle_cast/2, handle_call/3]).
-behavior(gen_server).

%-record(name, {id, name}).

%-record(table, {name_lst, maxid, readnum, writenum}).

-include("name.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

%% Customer Services API

add_name(Name) ->
    gen_server:call(?MODULE, {add_name, Name}, infinity).

del_name(Name) ->
    gen_server:call(?MODULE, {del_name, Name}, infinity).

get_name_byid(NameId) ->
    gen_server:call(?MODULE, {get_name, NameId}, infinity).

get_writenum() ->
    gen_server:call(?MODULE, {get_writenum}).

get_readnum() ->
    gen_server:call(?MODULE, {get_readnum}).

%% Callback Functions

%% 新建一个Table， 存放在LoopData中；
%% Table中包括名称表，最大Id，读表次数，写表次数。
init(_Arg) ->
    Table = name_db:create(),
    {ok, Table}.

terminate(_Reason, _LoopData) ->
    io:format("Test Over~n").

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData}.

handle_call({add_name, Name}, _From, LoopData) ->
    {Reply, NewLoopData} = name_db:insert(Name, LoopData),
    {reply, Reply, NewLoopData};

handle_call({del_name, Name}, _From, LoopData) ->
    {Reply, NewLoopData} = name_db:delete(Name, LoopData),
    {reply, Reply, NewLoopData};

handle_call({get_name, NameId}, _From, LoopData) ->
    {Reply, NewLoopData} = name_db:get(NameId, LoopData),
    io:format("Got Name is ~p~n", [Reply]),
    {reply, Reply, NewLoopData};

handle_call({get_writenum}, _From, LoopData) ->
    Num = LoopData#table.writenum,
    Reply = io:format("The Write Number is ~p~n", [Num]),
    {reply, Reply, LoopData};

handle_call({get_readnum}, _From, LoopData) ->
    Num = LoopData#table.readnum,
    Reply = io:format("The Read Number is ~p~n", [Num]),
    {reply, Reply, LoopData}.
