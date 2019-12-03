-module(mud_time).

%% 时间方面的公共设定，游戏里面用到的时间都是本地时间，即已经计算好时差的
%% 如果需要获取UTC，请调用erlang自带的函数

-compile([export_all]).
-export([
  test/2, test/3, test/4,
  binary_stamp/0,
  time_zone/0,
  unix_stamp/0, unix_stamp/1, unix_stamp/6,
  date_time/0, date_time/1,
  binary_date_time/0, binary_date_time/1
]).

%-define(SEC_19700101000000, 62167219200).
-define(TIME_ZONE, time_zone()).
-define(SECONDS_PER_MINUTE, 60).
-define(SECONDS_PER_HOUR, 3600).
-define(SECONDS_PER_DAY, 86400).

% 获取字符串戳（唯一字符串）
binary_stamp() ->
  %% R18之后，now()取消
  %% {A, B, C} = erlang:now(),
  {A, B, C} = erlang:timestamp(),
  F = A * 1000000000000 + B * 1000000 + C,
  iolist_to_binary(integer_to_list(F, 36)).

% 计算时区，+表示东，-表示西。如北京时间算出来的结果就是 8
time_zone() ->
  case get(gel_time_zone) of
    undefined ->
      {{_, _, D1}, {H1, _, _}} = erlang:localtime(),
      {{_, _, D2}, {H2, _, _}} = erlang:universaltime(),
      Tz = (D1 - D2) * 24 + H1 - H2,
      put(gel_time_zone, Tz), % 时区不会变化，只计算一次
      Tz;
    Tz -> Tz
  end.

% unix时间戳
unix_stamp() ->
  {A, B, _} = os:timestamp(),
  A * 1000000 + B.

%% 根据时间戳，把秒转换为其他单位，如
%% unix_stamp(60) 表示从 1970年1月1日0时0分到现在是多少分钟
%% unix_stamp(24*60*60) 表示从 1970年1月1日0时0分到现在是多少天
unix_stamp(Unit) when is_integer(Unit) -> unix_stamp() div Unit;

%%% 注释掉的这个实现没有下面的效率高
%%% 因为它是计算从 0年1月1日0时0分0秒开始的，多算了1970年.......
%unix_stamp( {{Year,Mon,Day},{Hour,Min,Sec}} ) ->
%DT = {{Year,Mon,Day},{Hour - ?TIME_ZONE,Min,Sec}},
%calendar:datetime_to_gregorian_seconds(DT) - ?SEC_19700101000000.
unix_stamp({{Year, Mon, Day}, {Hour, Min, Sec}}) ->
  Mon1 = Mon - 2,
  Hour1 = Hour - ?TIME_ZONE,
  {Mon2, Year1} =
    if
      Mon1 > 0 -> {Mon1, Year};
      Mon1 =< 0 -> {Mon1 + 12, Year - 1}
    end,
  Ret = ((Year1 div 4) - (Year1 div 100) + (Year1 div 400) + 367 * Mon2 div 12 + Day)
    + Year1 * 365 - 719499,
  %% Ret是日期，转换成秒
  ((Ret * 24 + Hour1) * 60 + Min) * 60 + Sec.

unix_stamp(Year, Mon, Day, Hour, Min, Sec) ->
  unix_stamp({{Year, Mon, Day}, {Hour, Min, Sec}}).



%% 日期，返回 20160101 的格式
date() ->
  {{Y, M, D}, _} = erlang:localtime(),
  Y * 10000 + M * 100 + D.


% 获取年月日
date_time() -> erlang:localtime().

%%% 注释掉的这个实现没有下面的效率高
%%% 因为它是计算从 0年1月1日0时0分0秒开始的，多算了1970年.......
%datetime(Sec) ->
%calendar:gregorian_seconds_to_datetime(Sec + ?SEC_19700101000000).
date_time(Sec) when is_integer(Sec) andalso Sec >= 0 ->
  Sec1 = Sec + ?TIME_ZONE * ?SECONDS_PER_HOUR,
  {N, Y, R} = calendar:gregorian_days_to_date(Sec1 div ?SECONDS_PER_DAY),
  {{N + 1970, Y, R}, calendar:seconds_to_time(Sec1 rem ?SECONDS_PER_DAY)}.

%% 返回 “年-月-日 时:分:秒”，如 2016-01-01 23:23:23
binary_date_time() ->
  binary_date_time(erlang:localtime()).
binary_date_time(Sec) when is_integer(Sec) ->
  binary_date_time(date_time(Sec));
binary_date_time({{N, Y, R}, {S, F, M}}) ->
  erlang:iolist_to_binary(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
    [N, Y, R, S, F, M])).

%% 返回 “年月日时分秒”，如 20160101232323
binary_date_time2() ->
  binary_date_time2(erlang:localtime()).
binary_date_time2(Sec) when is_integer(Sec) ->
  binary_date_time2(date_time(Sec));
binary_date_time2({{N, Y, R}, {S, F, M}}) ->
  erlang:iolist_to_binary(io_lib:format("~4.10.0B~2.10.0B~2.10.0B~2.10.0B~2.10.0B~2.10.0B",
    [N, Y, R, S, F, M])).


%% 返回 “年-月-日”，如 2016-01-01
binary_date() ->
  binary_date(erlang:localtime()).
binary_date(Sec) when is_integer(Sec) ->
  binary_date(date_time(Sec));
binary_date({{N, Y, R}, _}) ->
  erlang:iolist_to_binary(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B", [N, Y, R])).
%% 返回 “年月日”，如 20160101
binary_date2() ->
  binary_date2(erlang:localtime()).
binary_date2(Sec) when is_integer(Sec) ->
  binary_date2(date_time(Sec));
binary_date2({{N, Y, R}, _}) ->
  erlang:iolist_to_binary(io_lib:format("~4.10.0B~2.10.0B~2.10.0B", [N, Y, R])).


%% 返回 “时:分:秒”，如 23:23:23
binary_time() ->
  binary_time(erlang:localtime()).
binary_time(Sec) when is_integer(Sec) ->
  binary_time(date_time(Sec));
binary_time({_, {S, F, M}}) ->
  erlang:iolist_to_binary(io_lib:format("~2.10.0B:~2.10.0B:~2.10.0B", [S, F, M])).
%% 返回 “时分秒”，如 232323
binary_time2() ->
  binary_time2(erlang:localtime()).
binary_time2(Sec) when is_integer(Sec) ->
  binary_time2(date_time(Sec));
binary_time2({_, {S, F, M}}) ->
  erlang:iolist_to_binary(io_lib:format("~2.10.0B~2.10.0B~2.10.0B", [S, F, M])).


% 测试效率
test(M, F) ->
  test(M, F, undefined).
test(M, F, A) ->
  test(M, F, A, 100000).
test(M, F, A, T) ->
  {_, A1, B1} = os:timestamp(),
  test_func(M, F, A, abs(T)),
  {_, A2, B2} = os:timestamp(),
  C1 = A1 * 1000 + B1 div 1000,
  C2 = A2 * 1000 + B2 div 1000,
  io:format("run ~p times, cost : ~p ms~n", [T, C2 - C1]).

test_func(_, _, _, 0) -> ok;
test_func(M, F, A, T) ->
  case A of
    undefined -> M:F();
    _ -> apply(M, F, [A])
  end,
  test_func(M, F, A, T - 1).



