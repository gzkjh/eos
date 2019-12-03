%%%-------------------------------------------------------------------
%%% @author Commando
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 基于ranch的tcp连接模块
%%% @end
%%% Created : 29. 七月 2019 16:58
%%%-------------------------------------------------------------------
-module(gateway_ranch_tcp).
-author("Commando").

-include("public.hrl").

%% API
-export([start_link/1, start_ranch/1]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% 启动服务
%% @end
%%--------------------------------------------------------------------
%%-spec(start_link() ->
%%  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Opt) ->
  %% 如果需要简单实现，那么确保 ranch 启动了就可以了
  %% 这里是为了把 ranch_sup 挂在 gateway_sup 下，所以才自定义ranch的启动
  spawn(?MODULE, start_ranch, [Opt]),
  %% 自身不启动，返回ignore
  ?ignore.

%% 启动 ranch，把它挂在 gateway_sup 下
start_ranch(Opt) ->
  Handle = maps:get(mod_linker, Opt, linker_ranch_tcp), %% 默认的数据处理进程
  Port = maps:get(port, Opt, 0), %% 端口，默认0

  %% 确保 ranch 已经启动
  RanchSupSpec = {ranch_sup, {ranch_sup, start_link, []},
    permanent, 5000, supervisor, [ranch_sup]},
  ok = case supervisor:start_child(gateway_sup, RanchSupSpec) of
         {ok, _} -> ok;
         {error, {{already_started, _Pid}, _Child}} -> ok;
         Err ->
           ?error("start ranch fail:~p", [Err]),
           error
       end,

  ListenerSpec = ranch:child_spec(
    make_ref(),  %% 内部名字，如果从sup找出来，要用 {ranch_listener_sup, XXX} 来找
    ranch_tcp,
    #{
      num_acceptors => 30, % 监听进程数量
      max_connections => 8196,
      socket_opts => [{port, Port}]
    },
    Handle,  %% 启动进程的模块 start_link/4
    Opt    %% 参数
  ),
  {ok, _} = supervisor:start_child(gateway_sup, ListenerSpec),
  ?notice("~p listen on ~p", [?MODULE, Port]),
  ok.




