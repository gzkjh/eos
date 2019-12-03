%%%-------------------------------------------------------------------
%% @doc mud top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mud_sup).
-include("child_specs.hrl").
-include("public.hrl").
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).


start_link() ->
  %% 先把自己启动起来，作为根节点
  {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),

  %% 读取配置
  Conf = mud_config:load_term(),
  Trees = mud_proplists:get_value_recursive([mud, tree], Conf, []),

  %% 根据配置启动进程
  start_children(Pid, Trees),

  {ok, Pid}.

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
  SupFlags = #{strategy => one_for_one,  % 重启策略
    intensity => 3, % 最大重启次数
    period => 5}, % 时间段，单位：秒
  {ok, {SupFlags, []}}.

%% internal functions

%% 启动所有子进程
start_children(Sup, Children) ->
  lists:foreach(
    fun(Child) ->
      ok = start_child(Sup, Child)
    end,
    Children),
  ok.

%% 启动单个子进程
start_child(Sup, Child) ->
  case Child of
    %% id 是必须的
    #{id:=Name} ->
      %% 启动参数
      {Module, Fun, Args} =
        case maps:get(start, Child, Name) of
          M when is_atom(M) -> {M, start_link, []};
          {M, A} -> {M, start_link, A};
          {M, F, A} -> {M, F, A};
          ?undefined -> {Name, start_link, []}
        end,
      %% 进程类型，默认是 worker
      Type = maps:get(type, Child, worker),

      %% 拼装成参数
      ChildSpec = #{
        id => Name,       % 内部名字
        start => {Module, Fun, Args},      % 启动函数
        restart => permanent,   % 重启策略
        shutdown => 5000, % 关闭时间，单位：毫秒
        type => Type,       % 类型：worker或者supervisor
        modules => [Module]   % 回调模块
      },

      {ok, Pid} = supervisor:start_child(Sup, ChildSpec),

      %% 看看它的子进程
      Children = maps:get(children, Child, []),
      start_children(Pid, Children);
    _ ->
      lager:error("start child fail ~p", [Child]),
      error
  end.




