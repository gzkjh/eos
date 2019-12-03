%% 扩展的proplists函数
-module(mud_proplists).

-include("public.hrl").

-export([
  get_value_recursive/3, get_value_recursive/2, get_value_multi/2]).


%% 按层级递归获取数据
-spec get_value_recursive(KeyList, Data) -> any() when
  KeyList :: list(), Data :: list().
get_value_recursive(KeyList, Data) -> get_value_recursive(KeyList, Data, ?undefined).

-spec get_value_recursive(KeyList, Data, Default) -> any() when
  KeyList :: list(), Data :: list(),
  Default :: term().
get_value_recursive([], Data, _Default) -> Data;
get_value_recursive([Key | Rest], Data, Default) when is_list(Data) ->
  case proplists:get_value(Key, Data) of
    NewData when NewData =/= ?undefined ->
      get_value_recursive(Rest, NewData, Default);
    _ -> Default
  end;
get_value_recursive(_Key, _Data, Default) ->
  Default.

%% 同时获取多个值

-spec get_value_multi(KeyLista :: [T], Data :: list()) -> list() when
  T :: {term(), term()}|term().
get_value_multi(KeyList, Data) when is_list(KeyList) ->
  Out = lists:foldl(
    fun(MxKey, Ret) ->
      R = case MxKey of
            {Key, Default} -> proplists:get_value(Key, Data, Default);
            Key -> proplists:get_value(Key, Data)
          end,
      [R | Ret]
    end,
    [], KeyList),
  lists:reverse(Out).
