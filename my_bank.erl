-module(my_bank).
-compile(export_all).

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

new_account(Who) -> gen_server:call(?MOUDLE, {new, Who}).
deposit(Who, Amount) -> gen_server:call(?MODULE, {add, Who, Amount}).
withdraw(Who, Amount) -> gen_server:call(?MODULE, {remove, Who, Amount}).


