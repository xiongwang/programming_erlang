-module(my_bank).
-compile(export_all).
-behaviour(gen_server).

%% -define(my_bank, ?MODULE).

%% API
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).
new_account(Who) -> gen_server:call(?MOUDLE, {new, Who}).
deposit(Who, Amount) -> gen_server:call(?MODULE, {add, Who, Amount}).
withdraw(Who, Amount) -> gen_server:call(?MODULE, {remove, Who, Amount}).


init([]) -> {ok, ets:new(?MOUDLE, [])}.

%% process the new_accout/1
handle_call({new, Who}, _From, Tab) ->
    Reply = case ets:lookup(Tab, Who) of
        [] -> ets:insert(Tab, {Who, 0}),
            {welcome, Who};
        [_] -> {Account exists!}
    end,
    {reply, Reply, Tab};

%% process the deposit/2
handle_call({add, Who, X}, _From, Tab) ->
    Reply = case ets:lookup(Tab, Who) of
        %% once if he's not a customer
        [] -> not_a_customer;
        [{Who, Balance}] ->
            NewBalance = Balance + X,
            ets:insert(Tab, {Who, NewBalance}),
            {thanks, Who, your_balance_is, NewBalance}
    end,
    {reply, Reply, Tab};

%% process the remove/3
handle_call({remove, Who, X}, _From, Tab) ->
    Reply = case ets:lookup(Tab, Who) of
        [] -> not_a_customer;
        [{Who, Balance}] when X =< Balance ->
            NewBalance = Balance - X,
            ets:insert(Tab, {Who, NewBalance}),
            {thanks, Who, your_balance_is, NewBalance};
        [{Who, Balance}] ->
            {sorry, Who, your_only_have, Balance, in_the_bank}
    end,
    {reply, Reply, Tab};

handle_call(stop, _From, Tab) ->
    {stop, normal, stopped, Tab}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, Extra) -> {ok, State}.

