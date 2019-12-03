%%%-------------------------------------------------------------------
%% @doc login top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(login_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(CHILD(MOD), #{
  id => MOD,
  start => {MOD, start_link, []},
  restart => permanent, % 重启策略：一直重启
  shutdown => 5000, % 5秒时间关闭
  type => worker,
  modules => [MOD]
}).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

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
  SupFlags = #{strategy => one_for_one,
    intensity => 5,
    period => 10},
  ChildSpecs = [
    ?CHILD(login_debug)
  ],
  {ok, {SupFlags, ChildSpecs}}.

%% internal functions
