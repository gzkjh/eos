%% coding: utf-8
%% 读取proplists格式的配置文件
%% key支持list递归读取
-module(mud_config).

-include("public.hrl").


-export([load_term/0, load_term/1, load_json/0, load_json/1, lookup/1]).

%% 默认的配置文件
-define(CONFIG_FILE, "./etc/game.config").


%% @doc
%% 加载erlang格式的配置文件
%% @end
load_term() -> load_term(?CONFIG_FILE).
-spec load_term(Filename) -> Maps when Filename :: string(), Maps :: term().
load_term(Filename) ->
  case file:consult(Filename) of
    {ok, Data} -> Data;
    {error, Reason} ->
      lager:error("Fail to load ~p reason: ~p", [Filename, Reason]),
      #{}
  end.

%% @doc
%% 加载json格式的配置文件
%% @end
load_json() -> load_json(?CONFIG_FILE).
load_json(Filename) ->
  case file:read_file(Filename) of
    {ok, Bin} -> jsx:decode(Bin);
    {error, Reason} ->
      lager:error("Fail to load ~p reason: ~p", [Filename, Reason]),
      #{}
  end.


lookup(Key) ->
  Data = load_term(),
  mud_proplists:get_value_recursive([Key], Data).


%%%% 把json格式的配置写入ets，方便查询
%%settings_to_ets(Json) ->
%%  ets:new(?ets_settings,
%%    [set, named_table, public, {read_concurrency, true}]),
%%
%%  Keys = maps:keys(Json),
%%  lists:foreach(
%%    fun(Key) ->
%%      Value = maps:get(Key, Json),
%%      lager:notice("Load ~p = ~p ... Done!", [Key, Value]),
%%      ets:insert(?ets_settings, {Key, Value})
%%    end, Keys),
%%  ok.
