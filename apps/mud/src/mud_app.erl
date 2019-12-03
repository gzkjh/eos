%%%-------------------------------------------------------------------
%% @doc mud public API
%% @end
%%%-------------------------------------------------------------------

-module(mud_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  %% 这里打开可以看看各种颜色
%%  lager:debug("debug debug debug"),
%%  lager:info("info info info"),
%%  lager:notice("notice notice notice"),
%%  lager:warning("warning warning warning"),
%%  lager:error("error error error"),
%%  lager:critical("critical critical critical"),
%%  lager:alert("alert alert alert"),
%%  lager:emergency("emergency emergency emergency"),
  mud_sup:start_link().


stop(_State) ->
  ok.

%% internal functions

