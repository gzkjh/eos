%%%-------------------------------------------------------------------
%%% @author Commando
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 八月 2019 10:31
%%%-------------------------------------------------------------------
-module(mud_action).
-author("Commando").

-behaviour(gen_server).

-include("public.hrl").
%% API
-export([start_link/0, register_action/3, take_action/2, take_action/3, take_action/4]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).


-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% 获取-行为
get_action(Actor, Act) ->
  %% todo 先从actor身上找私有action，没有的话再从公共里面找
  {Act, funs}.

%% 执行-行为
take_action(Actor, Act) -> take_action(Actor, Act, #{}, #{}).
take_action(Actor, Act, RawArgs) -> take_action(Actor, Act, RawArgs, #{}).
take_action(Actor, Act, RawArgs, Opts) -> ok.

%% 注册通用行为（target是原子），将注册在ets表
register_action(Target, Act, AllMods) when is_atom(Target) ->
  Funs = [Fun || Mod <- AllMods, Fun = get_handle_action(Mod), Fun =/= ?undefined],
  case ets:insert_new(Target, {Act, Funs}) of
    ?true -> skip;
    ?false -> ?warning("register action false ets:~p act:~p", [Target, Act])
  end,
  ok;
%% 注册对象私有行为（target是map），将注册在map里面
register_action(Target, Act, Mod) when is_map(Target) ->
  Acts = maps:get(actions, Target, #{}),
  ok.


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->

  %% 创建一个用户行为记录
  ets:new(ets_user_action,
    [set, named_table, protected, {read_concurrency, true}]),
  register_proto_action(),

  %% 创建一个monster的行为记录
  ets:new(ets_map_action,
    [set, named_table, protected, {read_concurrency, true}]),

  %% 创建一个npc的行为记录
  ets:new(ets_npc_action,
    [set, named_table, protected, {read_concurrency, true}]),

  %% 创建一个monster的行为记录
  ets:new(ets_monster_action,
    [set, named_table, protected, {read_concurrency, true}]),

  %% 创建一个boss的行为记录
  ets:new(ets_boss_action,
    [set, named_table, protected, {read_concurrency, true}]),

  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

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
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

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
    State :: #state{}) -> term()).
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
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% 从模块里面获取handle_action/4函数
%% handle_action格式：fun(Actor, Act, RawArgs, Opts)
get_handle_action(Mod) ->
  case erlang:module_loaded(Mod) of
    ?true -> skip;
    _ -> code:ensure_loaded(Mod)
  end,
  case erlang:function_exported(Mod, handle_action, 4) of
    ?true -> fun Mod:handle_action/4;
    ?false -> ?undefined
  end.

%% 从proto信息里面提取action信息
register_proto_action() ->
  %% 从cmd里面提取就可以了
  AllInfo = all_pb:get_msg_defs(),

  %% 把enum提取出来
  AllEnum = lists:foldl(
    fun(E, Inn) ->
      case E of
        {{enum, N1}, L} ->
          N2 = atom_to_list(N1),
          P = string:rchr(N2, $.),
          N3 = string:substr(N2, 1, P - 1),
          N4 = list_to_existing_atom(N3),
          [[N4 | L] | Inn];
        _ -> Inn
      end
    end,
    [], AllInfo),

  ?notice("all enum ~p", [AllEnum]),
  % 把mod提取出来
  AllMods = get_mods_in_enum(AllEnum, []),
  ?notice("all actions ~p", [AllMods]),
  lists:foreach(
    fun(E) ->
      case E of
        {0, _, _} = Act ->
          ?warning("skip Action for no proto_id : ~p", [Act]),
          skip;
        {_, _, []} = Act ->
          ?warning("skip Action for empty mods : ~p", [Act]),
          skip;
        {Pt, Rec, _Mods} = Act ->
          ?notice("register Action ~p", [Act]),
          ets:insert_new(ets_user_action, {Pt, Act}),
          ets:insert_new(ets_user_action, {Rec, Act})
      end
    end, AllMods),
  ok.

get_mods_in_enum([], Ret) -> Ret;
get_mods_in_enum([H | T], Ret) ->
  R = lists:foldl(
    fun(E, {Act, R, List} = Inn) ->
      case E of
        E when is_atom(E) -> {Act, E, List};
        {option, _, _} -> Inn;
        {unknow, _} -> Inn;
        {proto_id, Pt} -> {Pt, R, List};
        {_, 0} -> Inn;
        {M, 1} -> {Act, R, [M | List]};
        _ -> Inn
      end
    end,
    {0, ?undefined, []}, H),
  get_mods_in_enum(T, [R | Ret]).




