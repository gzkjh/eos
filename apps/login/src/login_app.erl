%%%-------------------------------------------------------------------
%% @doc login public API
%% @end
%%%-------------------------------------------------------------------

-module(login_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    login_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
