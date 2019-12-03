%%%-------------------------------------------------------------------
%%% @author Commando
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 七月 2019 15:39
%%%-------------------------------------------------------------------
-module(mud_server).
-author("Commando").
-include("public.hrl").
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3, start_link/3, start/2, start_link/2]).

-record(wrap_state, {
  state = ?undefined :: term(),
  handle_call = [] :: [fun((Request :: term(), From :: {pid(), Tag :: term()}, Continue :: term(), State :: term()) -> term())],
  handle_cast = [] :: [fun((Request :: term(), Continue :: term(), State :: term()) -> term())],
  handle_info = [] :: [fun((Request :: term(), Continue :: term(), State :: term()) -> term())],
  init_module = ?undefined :: atom()
}).

%%%===================================================================
%%% API
%%%===================================================================

-callback init(Args :: term()) -> {ok, State :: term()}.
-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    Continue :: term(), State :: term()) ->
  {continue, Continue :: term(), NewState :: term()} |
  {reply, Reply :: term(), NewState :: term()} |
  {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
  {noreply, NewState :: term()} |
  {noreply, NewState :: term(), timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
  {stop, Reason :: term(), NewState :: term()}.
-callback handle_cast(Request :: term(), Continue :: term(), State :: term()) ->
  {continue, Continue :: term(), NewState :: term()} |
  {noreply, NewState :: term()} |
  {noreply, NewState :: term(), timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: term()}.
-callback handle_info(Info :: timeout | term(), Continue :: term(), State :: term()) ->
  {continue, Continue :: term(), NewState :: term()} |
  {noreply, NewState :: term()} |
  {noreply, NewState :: term(), timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: term()}.

-optional_callbacks([handle_info/3]).


%%%===================================================================
%%% 自身的实现
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
%%-spec(start_link() ->
%%  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Module, Args) ->
  gen_server:start_link(?MODULE, [Module, Args], []).
start_link(Name, Module, Args) ->
  gen_server:start_link(Name, ?MODULE, [Module, Args], []).


start(Module, Args) ->
  gen_server:start(?MODULE, [Module, Args], []).

%%--------------------------------------------------------------------
%% @doc
%% 初始化
%%
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: list()) -> Result when
  Result :: {ok, State}|{ok, State, Timeout}|{stop, Reason},
  State :: term(),
  Timeout :: integer()|infinity,
  Reason :: term().
init([MainMod, Args]) ->
  State =
    case MainMod:init(Args) of
      {ok, #{state:=S, handle_call:=FunsCall, handle_cast:=FunsCast, handle_info:=FunsInfo}}
        when is_list(FunsCall), is_list(FunsCast), is_list(FunsInfo) ->
        #wrap_state{
          state = S,
          handle_call = FunsCall,
          handle_cast = FunsCast,
          handle_info = FunsInfo,
          init_module = MainMod
        };
      _ -> throw(<<"init false!">>)
    end,
  {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #wrap_state{}) ->
  {reply, Reply :: term(), NewState :: #wrap_state{}} |
  {reply, Reply :: term(), NewState :: #wrap_state{}, timeout() | hibernate} |
  {noreply, NewState :: #wrap_state{}} |
  {noreply, NewState :: #wrap_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #wrap_state{}} |
  {stop, Reason :: term(), NewState :: #wrap_state{}}).

%% 还没绑定模块
handle_call(_Request, _From, #wrap_state{handle_call = []} = Wrap) ->
  lager:warning("no handle_call fun inited in ~p.", [Wrap#wrap_state.init_module]),
  {reply, ?undefined, Wrap};
handle_call(Request, From, #wrap_state{handle_call = FunList} = Wrap) ->
  loop_call(Request, From, #{}, Wrap, FunList).

%% 循环处理
%% 如果上一个函数依然要求continue，但是已经没有下一个函数了，那就返回 undefined
loop_call(Request, From, Continue, #wrap_state{state = State} = Wrap, [Fun | NextFuns]) ->
  case Fun(Request, From, Continue, State) of
    %% 扩展了continue的处理
    {continue, NewContinue, NewState} when NextFuns =:= [] ->
      %% 如果已经没有后续函数，但是依然有continue，那么continue将作为reply返回
      {reply, NewContinue, Wrap#wrap_state{state = NewState}};
    {continue, NewContinue, NewState} ->
      loop_call(Request, From, NewContinue, Wrap#wrap_state{state = NewState}, NextFuns);
    %% 以下几个是标准的handle_call结果
    {reply, Reply, NewState} ->
      {reply, Reply, Wrap#wrap_state{state = NewState}};
    {reply, Reply, NewState, Timeout} ->  % 这里已经包括了 Timeout = hibernate 的情况了
      {reply, Reply, Wrap#wrap_state{state = NewState}, Timeout};
    {noreply, NewState} ->
      {noreply, Wrap#wrap_state{state = NewState}};
    {noreply, NewState, Timeout} -> % 这里已经包括了 Timeout = hibernate 的情况了
      {noreply, Wrap#wrap_state{state = NewState}, Timeout};
    {stop, Reason, NewState} ->
      {stop, Reason, Wrap#wrap_state{state = NewState}};
    {stop, Reason, Reply, NewState} ->
      {stop, Reason, Reply, Wrap#wrap_state{state = NewState}}
  end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #wrap_state{}) ->
  {noreply, NewState :: #wrap_state{}} |
  {noreply, NewState :: #wrap_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #wrap_state{}}).
handle_cast(_Request, #wrap_state{handle_cast = []} = Wrap) ->
  {noreply, Wrap};
handle_cast(Request, #wrap_state{handle_cast = FunList} = Wrap) ->
  loop_cast(Request, #{}, Wrap, FunList).


loop_cast(Request, Continue, #wrap_state{state = State} = Wrap, [Fun | NextFuns]) ->
  case Fun(Request, Continue, State) of
    %% 扩展了continue的处理
    {continue, _NewContinue, NewState} when NextFuns =:= [] ->
      {noreply, Wrap#wrap_state{state = NewState}};
    {continue, NewContinue, NewState} ->
      loop_cast(Request, NewContinue, Wrap#wrap_state{state = NewState}, NextFuns);
    %% 以下几个是标准的handle_call结果
    {noreply, NewState} ->
      {noreply, Wrap#wrap_state{state = NewState}};
    {noreply, NewState, Timeout} -> % 这里已经包括了 Timeout = hibernate 的情况了
      {noreply, Wrap#wrap_state{state = NewState}, Timeout};
    {stop, Reason, NewS} ->
      {stop, Reason, Wrap#wrap_state{state = NewS}}
  end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #wrap_state{}) ->
  {noreply, NewState :: #wrap_state{}} |
  {noreply, NewState :: #wrap_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #wrap_state{}}).
handle_info(_Request, #wrap_state{handle_info = []} = Wrap) ->
  {noreply, Wrap};
handle_info(Request, #wrap_state{handle_info = Funlist} = Wrap) ->
  loop_info(Request, #{}, Wrap, Funlist).


loop_info(Request, Continue, #wrap_state{state = State} = Wrap, [Fun | NextFuns]) ->
  case Fun(Request, Continue, State) of
    %% 扩展了continue的处理
    {continue, _NewContinue, NewState} when NextFuns =:= [] ->
      {noreply, Wrap#wrap_state{state = NewState}};
    {continue, NewContinue, NewState} ->
      loop_info(Request, NewContinue, Wrap#wrap_state{state = NewState}, NextFuns);
    %% 以下几个是标准的handle_call结果
    {noreply, NewState} ->
      {noreply, Wrap#wrap_state{state = NewState}};
    {noreply, NewState, Timeout} -> % 这里已经包括了 Timeout = hibernate 的情况了
      {noreply, Wrap#wrap_state{state = NewState}, Timeout};
    {stop, Reason, NewState} ->
      {stop, Reason, Wrap#wrap_state{state = NewState}}
  end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #wrap_state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, Wrap :: #wrap_state{},
    Extra :: term()) ->
  {ok, NewState :: #wrap_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, Wrap, _Extra) ->
  {ok, Wrap}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

