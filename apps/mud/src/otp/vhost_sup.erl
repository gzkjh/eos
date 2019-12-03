%%%-------------------------------------------------------------------
%%% @author Commando
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 游戏服监控树
%%% @end
%%% Created : 25. 七月 2019 11:21
%%%-------------------------------------------------------------------
-module(vhost_sup).
-author("Commando").

-behaviour(supervisor).

%% Supervisor callbacks
-export([init/1, start_link/0]).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
%%-spec(start_link() ->
%%  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([]) ->
  SupFlags = #{
    strategy => one_for_one,  % 重启模式
    intensity => 3, % 最大尝试次数
    period => 5   % 时间周期，单位：秒
  },


  {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
