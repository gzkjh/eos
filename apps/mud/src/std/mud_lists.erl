%% 扩展的list函数
-module(mud_lists).

-include("game_logger.hrl").

%-compile([export_all]).
-export([
  unseq/2, unseq/3,
  shuffle/1,
  delete/2,
  replace/3, replace/4,
  replace_by_pos/3, add_unique/2, to_list/1]).

%% 按位置换掉列表元素
replace_by_pos(List, N, Value) ->
  {H, [_Remove | T]} = lists:split(N, List),
  lists:append([H, [Value], T]).


%% 替换列表所有的Value为NewValue
-spec replace(List1, Value, NewValue) -> List2 when
  List1 :: list(),
  Value :: term(),
  NewValue :: term(),
  List2 :: list().

replace(List, Value, NewValue) ->
  replace_all(List, Value, NewValue, []).

replace_all([], _Value, _NewValue, Nlist) -> lists:reverse(Nlist);
replace_all([H | T], Value, NewValue, Nlist) ->
  if
    H =:= Value -> replace_all(T, Value, NewValue, [NewValue | Nlist]);
    H =/= Value -> replace_all(T, Value, NewValue, [H | Nlist])
  end.


%% 查找List中Count数量的Value，替换成NewValue，并返回新的列表
%% 若Count小于0，则全替换
replace(List, Value, NewValue, Count) ->
  replace(List, Value, NewValue, Count, []).


replace([], _Value, _NewValue, _Count, Nlist) -> lists:reverse(Nlist);
replace([H | T], Value, NewValue, Count, Nlist) ->
  {Nlist2, Count2} = case H =:= Value of
                       true -> {[NewValue | Nlist], Count - 1};
                       false -> {[H | Nlist], Count}
                     end,
  case Count2 == 0 of
    true -> lists:append(lists:reverse(Nlist2), T);
    false -> replace(T, Value, NewValue, Count2, Nlist2)
  end.


%% 如果列表里面没有，则添加
add_unique(List, Value) ->
  case lists:member(Value, List) of
    true -> List;
    false -> [Value | List]
  end.

%% 删除列表里面相同的所有元素，lists里面的delete只删除1个
-spec delete([Value|Rest], Value) -> List when
  Value :: term(),
  List :: Rest,
  Rest :: list().

delete(List, Value) ->
  delete_self(List, Value, []).

delete_self([], _Value, Nlist) -> lists:reverse(Nlist);
delete_self([Value | T], Value, Nlist) -> delete_self(T, Value, Nlist);
delete_self([H | T], Value, Nlist) -> delete_self(T, Value, [H | Nlist]).


%% 生成一个乱序的列表，跟 lists:seq 对应
-spec unseq(integer(), integer()) -> list().
unseq(From, To) when is_integer(From) andalso is_integer(To) -> unseq(From, To, 1).
unseq(From, To, Incr)
  when is_integer(From) andalso is_integer(To) andalso is_integer(Incr)
  -> shuffle(lists:seq(From, To, Incr)).

%% 洗牌，把列表顺序打乱
-spec shuffle(list()) -> list().
shuffle(List) when is_list(List) -> shuffle_self(List, length(List), []).
shuffle_self(_, 0, Ret) -> Ret;
shuffle_self(List, Len, Ret) ->
  {HL, [H | T]} = lists:split(mud_utils:random(Len) - 1, List),
  shuffle_self(HL ++ T, Len - 1, [H | Ret]).



to_list(X) when is_list(X) -> X;
to_list(X) when is_binary(X) ->
  binary_to_list(X);
to_list(X) when is_integer(X) ->
  integer_to_list(X);
to_list(X) when is_atom(X) ->
  atom_to_list(X);
to_list(X) when is_float(X) ->
  float_to_list(X).


