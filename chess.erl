%============================================================================
% Filename		: chess.erl
% Description	: tic-tac-toe game on erlang shell
% Author		: Wang, Xiong
% Email			: xiongwang@live.com
% Last Modified	: 10/4/2013 10:37:57 PM
%============================================================================

-module(chess).
-behaviour(gen_server).

-compile(export_all).
-export([
    init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {
          board = {1, 2, 3, 4, 5, 6, 7, 8, 9},
          round = 0
         }).

start() ->
    io:format("Chess Game Started!~n"),
    start_link([]),
    ok.

a(Pos) ->
    gen_server:cast(?SERVER, {a, [Pos]}).

b(Pos) ->
    gen_server:cast(?SERVER, {b, [Pos]}).

start_link(Arg) ->
    {ok, _Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, Arg, []).


%% @return ok
stop() ->
    self() ! stop.

%% @param Callback = fun/1 | {Module, Func}/1
call(Callback) ->
    gen_server:call(?SERVER, {'$call', Callback}).

%% @param Callback = fun/1 | {Module, Func}/1
cast(Callback) ->
    gen_server:cast(?SERVER, {'$cast', Callback}).

call(Flag, Info) ->
    gen_server:call(?SERVER, {Flag, Info}).

cast(Flag, Info) ->
    gen_server:cast(?SERVER, {Flag, Info}).

%%====================================================================
%% Callback functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init(_) ->
    erlang:process_flag(trap_exit, true),

    State = #state{},
    put(board, State#state.board),
    put(round, State#state.round),
    
    play_game(),
    {ok, State}.
%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Param: From = {pid(), Tag}
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({'$call', Callback}, _From, State) ->
    Reply = try Callback(State) catch 
        _Type:Why ->
            io:format("Error in handle_call~n"),
            {error, Why}
    end,
    {reply, Reply, State};

handle_call({stop, Reason}, _From, State) ->
    {stop, Reason, ok, State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast({'$cast', Callback}, State) ->
    try Callback(State) catch 
        _Type:_Why ->
            io:format("Error in handle_cast~n")
    end,
    {noreply, State};

handle_cast({a, [Pos]}, State) ->
    do_a(Pos),
    {noreply, State};

handle_cast({b, [Pos]}, State) ->
    do_b(Pos),
    {noreply, State};

handle_cast(_Info, State) -> 
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({'EXIT', _Pid, _Why}, State) -> 
    {noreply, State};

handle_info(stop, State) ->
    {stop, normal, State};

handle_info(_Info, State) -> 
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
%% terminate(normal, State) ->
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Function: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ------------------------------------------------------------------
%%  Private Functions
%% ------------------------------------------------------------------
play_game() ->
    print_board(),
    Round = get(round),
    case (Round rem 2 =:= 0) of
        true ->
            io:format("Next Player: A~n");
        false ->
            io:format("Next Player: B~n")
    end.

print_board() ->
    Board = get(board),
    print_board(Board).
print_board(Board) ->
    {A, B, C, D, E, F, G, H ,I} = Board,
    Round = get(round),
    io:format("Round: ~p~n", [Round]),
    io:format("   ~p   ~p   ~p~n", [A, B, C]),
    io:format("   ~p   ~p   ~p~n", [D, E, F]),
    io:format("   ~p   ~p   ~p~n", [G, H, I]).


do_a(Pos) ->
    Board = get(board),
    move(a, Pos, Board).

do_b(Pos) ->
    Board = get(board),
    move(b, Pos, Board).

move(a, Pos, Board) ->
    case check_can_move(Pos, Board) of
        true ->
            NewBoard = setelement(Pos, Board, x),
            Round = get(round),
            put(round, Round + 1),
            put(board, NewBoard),
            %print_board(NewBoard),
            check_is_win(NewBoard);
        _ ->
            io:format("~nHEY, A, This Position Was Occupied!~n")
    end;
move(b, Pos, Board) ->
    case check_can_move(Pos, Board) of
        true ->
            NewBoard = setelement(Pos, Board, o),
            Round = get(round),
            put(round, Round + 1),
            put(board, NewBoard),
            %print_board(NewBoard),
            check_is_win(NewBoard);
        _ ->
            io:format("~nHEY, B, This Position Was Occupied!~n")
    end.

check_can_move(Pos, Board) ->
    Val = element(Pos, Board),
    Val >= 1 andalso Val =< 9.

check_is_win(Board) ->
    case do_check_is_win(Board) of
        {win, a} ->
            io:format("~n===============~nA WON THE GAME!!!!~n===============~n"),
            stop();
        {win, b} ->
            io:format("~n===============~nB WON THE GAME!!!!~n===============~n"),
            stop();
        {draw} ->
            io:format("~n===============~nPLAYER DRAW!~n==================~n"),
            stop();
        _ ->
            play_game()
    end.

do_check_is_win(Board) ->
    case Board of
        {x, x, x,
         _, _, _,
         _, _, _} -> {win, a};

        {_, _, _,
         x, x, x,
         _, _, _} -> {win, a};

        {_, _, _,
         _, _, _,
         x, x, x} -> {win, a};

        {x, _, _,
         x, _, _,
         x, _, _} -> {win, a};

        {_, x, _,
         _, x, _,
         _, x, _} -> {win, a};

        {_, _, x,
         _, _, x,
         _, _, x} -> {win, a};

        {x, _, _,
         _, x, _,
         _, _, x} -> {win, a};

        {_, _, x,
         _, x, _,
         x, _, _} -> {win, a};

        {o, o, o,
         _, _, _,
         _, _, _} -> {win, b};

        {_, _, _,
         o, o, o,
         _, _, _} -> {win, b};

        {_, _, _,
         _, _, _,
         o, o, o} -> {win, b};

        {o, _, _,
         o, _, _,
         o, _, _} -> {win, b};

        {_, o, _,
         _, o, _,
         _, o, _} -> {win, b};

        {_, _, o,
         _, _, o,
         _, _, o} -> {win, b};

        {o, _, _,
         _, o, _,
         _, _, o} -> {win, b};

        {_, _, o,
         _, o, _,
         o, _, _} -> {win, b};

        {A, B, C,
         D, E, F,
         G, H, I} when A =/= 1, B =/= 2, C =/= 3,
                       D =/= 4, E =/= 5, F =/= 6,
                       G =/= 7, H =/= 8, I =/= 9 ->
            {draw};

        _ ->
            {not_end}
    end.
