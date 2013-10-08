%============================================================================
% Filename		: chess_ai.erl
% Description	: tic-tac-toe game AI
% Author		: Wang, Xiong
% Email			: xiongwang@live.com
% Last Modified	: 10/5/2013 4:37:14 PM
%============================================================================

-module(chess_ai).
-behaviour(gen_server).

-export([
         start/0, stop/0, a/1
]).

-export([
    init/1,
    handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

-define(SERVER, ?MODULE).
-define(INFINITY, 100).
-define(DRAW, 0).
-define(WIN, -?INFINITY).
-define(LOSE, ?INFINITY).
-define(INPROGRESS, 1).
-define(DOUBLE_CONNECTED, 50).
-define(NOT_END, 1).


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

start_link(Arg) ->
    {ok, _Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, Arg, []).


%% @return ok
stop() ->
    stop(normal).
stop(Reason) ->
    gen_server:cast(?SERVER, {stop, Reason}).

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
    {stop, Reason, State};

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

handle_cast({stop, Reason}, State) ->
    {stop, Reason, State};

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
    Round = get(round),
    case (Round rem 2 =:= 0) of
        true ->
            print_board(),
            io:format("Next Player: A~n");
        false ->
            Pos = find_best_step(),
            io:format("AI is now calculating, Gonna Go Pos#~p~n", [Pos]),
            do_b(Pos)
    end.

print_board() ->
    Board = get(board),
    print_board(Board).
print_board(Board) ->
    {A, B, C, D, E, F, G, H ,I} = Board,
    Round = get(round),
    io:format("Round: ~p~n", [Round]),
    io:format("   ~p   ~p   ~p~n   ~p   ~p   ~p~n   ~p   ~p   ~p~n"
              , [A, B, C, D, E, F, G, H, I]).


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
            io:format("[HEY, A!], Position #~p Was Occupied!~n", [Pos])
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
            io:format("[HEY, B!], Position #~p Was Occupied!~n", [Pos])
    end.

check_can_move(Pos, Board) ->
    Val = element(Pos, Board),
    Val >= 1 andalso Val =< 9.

check_is_win(Board) ->
    case do_check_is_win(Board) of
        {win, a} ->
            io:format("~n===============~nA WON THE GAME!~n===============~n~n"),
            stop();
        {win, b} ->
            io:format("~n===============~nB WON THE GAME!~n===============~n~n"),
            stop();
        {draw} ->
            io:format("~n=================~nPLAYER DRAW!~n================~n~n"),
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

%% ---------------------------------
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

%% ---------------------------------
        {A, B, C,
         D, E, F,
         G, H, I} when A =/= 1, B =/= 2, C =/= 3,
                       D =/= 4, E =/= 5, F =/= 6,
                       G =/= 7, H =/= 8, I =/= 9 ->
            {draw};
%% ---------------------------------
        
        {o, o, _,
         _, _, _,
         _, _, _} -> {double_connected, b};
        
        {_, o, o,
         _, _, _,
         _, _, _} -> {double_connected, b};
        
        {_, _, _,
         o, o, _,
         _, _, _} -> {double_connected, b};
        
        {_, _, _,
         _, o, o,
         _, _, _} -> {double_connected, b};
        
        {_, _, _,
         _, _, _,
         o, o, _} -> {double_connected, b};
        
        {_, _, _,
         _, _, _,
         _, o, o} -> {double_connected, b};
        
        {o, _, _,
         o, _, _,
         _, _, _} -> {double_connected, b};
        
        {_, _, _,
         o, _, _,
         o, _, _} -> {double_connected, b};
        
        {_, o, _,
         _, o, _,
         _, _, _} -> {double_connected, b};
        
        {_, _, _,
         _, o, _,
         _, o, _} -> {double_connected, b};
        
        {_, _, o,
         _, _, o,
         _, _, _} -> {double_connected, b};
        
        {_, _, _,
         _, _, o,
         _, _, o} -> {double_connected, b};
        
        {o, _, _,
         _, o, _,
         _, _, _} -> {double_connected, b};
        
        {_, _, _,
         _, o, _,
         _, _, o} -> {double_connected, b};
        
        {_, _, o,
         _, o, _,
         _, _, _} -> {double_connected, b};
        
        {_, _, _,
         _, o, _,
         o, _, _} -> {double_connected, b};

%% ---------------------------------
        {x, x, _,
         _, _, _,
         _, _, _} -> {double_connected, a};
        
        {_, x, x,
         _, _, _,
         _, _, _} -> {double_connected, a};
        
        {_, _, _,
         x, x, _,
         _, _, _} -> {double_connected, a};
        
        {_, _, _,
         _, x, x,
         _, _, _} -> {double_connected, a};
        
        {_, _, _,
         _, _, _,
         x, x, _} -> {double_connected, a};
        
        {_, _, _,
         _, _, _,
         _, x, x} -> {double_connected, a};
        
        {x, _, _,
         x, _, _,
         _, _, _} -> {double_connected, a};
        
        {_, _, _,
         x, _, _,
         x, _, _} -> {double_connected, a};
        
        {_, x, _,
         _, x, _,
         _, _, _} -> {double_connected, a};
        
        {_, _, _,
         _, x, _,
         _, x, _} -> {double_connected, a};
        
        {_, _, x,
         _, _, x,
         _, _, _} -> {double_connected, a};
        
        {_, _, _,
         _, _, x,
         _, _, x} -> {double_connected, a};
        
        {x, _, _,
         _, x, _,
         _, _, _} -> {double_connected, a};
        
        {_, _, _,
         _, x, _,
         _, _, x} -> {double_connected, a};
        
        {_, _, x,
         _, x, _,
         _, _, _} -> {double_connected, a};
        
        {_, _, _,
         _, x, _,
         x, _, _} -> {double_connected, a};

        _ ->
            {not_end}
    end.

%% ---------------------------------------------------------
%%  AI
%% ---------------------------------------------------------
find_best_step() ->
    Board = get(board),
    Index = 1,
    BestValue = -?INFINITY,
    BestMoves = {1, 1, 1, 1, 1, 1, 1, 1, 1},
    Pos = do_get_best_pos(1, 10, Index, BestMoves, BestValue, Board),
    Pos.

do_get_best_pos(Max, Max, Index, BestMoves, _BestValue, _Board) ->
    NewIndex = 
    case Index > 1 of
        true ->
            random:uniform(Index);
        _ ->
            Index
    end,
    element(NewIndex, BestMoves);
do_get_best_pos(Count, Max, Index, BestMoves, BestValue, Board) ->
    case element(Count, Board) =:= Count of
        true ->
            NewBoard = setelement(Count, Board, o),
            Value = minSearch(NewBoard),
            if
                Value > BestValue ->
                    NewBestValue = Value,
                    NewIndex = 1,
                    NewBestMoves = setelement(NewIndex, BestMoves, Count);
                Value =:= BestValue ->
                    NewBestValue = Value,
                    NewIndex = Index + 1,
                    NewBestMoves = setelement(Index, BestMoves, Count);
                true ->
                    NewBestValue = BestValue,
                    NewIndex = Index,
                    NewBestMoves = BestMoves
            end,
            setelement(Count, NewBoard, Count);
        _ ->
            NewBestValue = BestValue,
            NewIndex = Index,
            NewBestMoves = BestMoves
    end,
    do_get_best_pos(Count + 1, Max, NewIndex, NewBestMoves, NewBestValue, Board).


% ----------------------------------------
maxSearch(Board) ->
    case game_state(Board) of
        ?DRAW ->
            0;
        ?INPROGRESS ->
            BestValue = -?INFINITY,
            do_max_search(1, 10, BestValue, Board);
        Other ->
            Other
    end.

do_max_search(Max, Max, BestValue, _Board) ->
    BestValue;
do_max_search(Count, Max, BestValue, Board) ->
    case element(Count, Board) =:= Count of
        true ->
            NewBoard = setelement(Count, Board, o),
            Value = minSearch(NewBoard),
            NewBestValue = 
            case Value > BestValue of
                true ->
                    Value;
                false ->
                    BestValue
            end,
            setelement(Count, NewBoard, Count);
        false ->
            NewBestValue = BestValue
    end,
    do_max_search(Count + 1, Max, NewBestValue, Board).

% ----------------------------------------
minSearch(Board) ->
    case game_state(Board) of
        ?DRAW ->
            0;
        ?INPROGRESS ->
            BestValue = ?INFINITY,
            do_min_search(1, 10, BestValue, Board);
        Other ->
            Other
    end.

do_min_search(Max, Max, BestValue, _Board) ->
    BestValue;
do_min_search(Count, Max, BestValue, Board) ->
    case element(Count, Board) =:= Count of
        true ->
            NewBoard = setelement(Count, Board, o),
            Value = maxSearch(NewBoard),
            NewBestValue = 
            case Value < BestValue of
                true ->
                    Value;
                false ->
                    BestValue
            end,
            setelement(Count, NewBoard, Count);
        false ->
            NewBestValue = BestValue
    end,
    do_min_search(Count + 1, Max, NewBestValue, Board).

%% @desc: check the board and return now status
%% @return: int
game_state(Board) ->
   case do_check_is_win(Board) of
       {draw} ->
           ?DRAW;
       {double_connected, b} ->
           -?DOUBLE_CONNECTED;
       {double_connected, a} ->
           ?DOUBLE_CONNECTED;
       {win, a} ->
           ?LOSE;
       {win, b} ->
           ?WIN;
       {not_end} ->
           ?INPROGRESS
   end.
