%% 常用小函数
-module(mud_utils).

%-compile([export_all]).
-export([range/3, flush/0, random_token/0, random/2, random/1, md5/1, binary_to_hex/1, random_string/1, sleep/1]).

%% 限制变量在某个范围
-spec range(integer(), integer(), integer()) -> integer().
range(X, Min, Max) when Min > Max ->
  range(X, Max, Min);
range(X, Min, Max) ->
  if
    X < Min -> Min;
    X > Max -> Max;
    true -> X
  end.

%% 清空进程消息队列
flush() ->
  receive _ -> flush()
  after 0 -> ok
  end.

%% 等待N毫秒
sleep(N) ->
  receive
  after N -> ok
  end.


%% 生成随机的32个字符串
random_token() ->
  Term = term_to_binary({node(), make_ref()}),
  md5(Term).

binary_to_hex(Bin) ->
  <<
    <<(hex(A)), (hex(B))>> || <<A:4, B:4>> <= Bin
  >>.

%% 最快的md5计算
md5(M) when is_list(M) -> md5(unicode:characters_to_binary(M));
md5(M) ->
  <<A1:4, A2:4, A3:4, A4:4, A5:4, A6:4, A7:4, A8:4,
    A9:4, A10:4, A11:4, A12:4, A13:4, A14:4, A15:4, A16:4,
    A17:4, A18:4, A19:4, A20:4, A21:4, A22:4, A23:4, A24:4,
    A25:4, A26:4, A27:4, A28:4, A29:4, A30:4, A31:4, A32:4>> = erlang:md5(M),
  <<(hex(A1)), (hex(A2)), (hex(A3)), (hex(A4)), (hex(A5)), (hex(A6)), (hex(A7)), (hex(A8)),
    (hex(A9)), (hex(A10)), (hex(A11)), (hex(A12)), (hex(A13)), (hex(A14)), (hex(A15)), (hex(A16)),
    (hex(A17)), (hex(A18)), (hex(A19)), (hex(A20)), (hex(A21)), (hex(A22)), (hex(A23)), (hex(A24)),
    (hex(A25)), (hex(A26)), (hex(A27)), (hex(A28)), (hex(A29)), (hex(A30)), (hex(A31)), (hex(A32))>>.

%% 10进制转16进制，效率最高
hex(0) -> $0;
hex(1) -> $1;
hex(2) -> $2;
hex(3) -> $3;
hex(4) -> $4;
hex(5) -> $5;
hex(6) -> $6;
hex(7) -> $7;
hex(8) -> $8;
hex(9) -> $9;
hex(10) -> $a;
hex(11) -> $b;
hex(12) -> $c;
hex(13) -> $d;
hex(14) -> $e;
hex(15) -> $f.

%% 10进制转16进制，这个比较好看，但是效率没上面的高
%%hex(X) -> element(X + 1, {$0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $a, $b, $c, $d, $e, $f}).
%% 10进制转16进制，这个也可以，但是效率还是没第一个高
%%hex(N) when N < 10 -> $0 + N;
%%hex(N) -> $a + (N - 10).


random(Num) when Num >= 1 -> random(1, Num).

-spec random(integer(), integer()) -> integer().
random(Min, Max) when Min > Max -> random(Max, Min);
random(Same, Same) -> Same;
random(Min, Max) ->
  %% 生成一个种子
%%  case get(gel_random_seed) of % 注意这里不要用 random_seed ，会跟 random 模块冲突
%%    undefined ->
%%      %% 旧写法, < erl 18
%%      %% <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
%%      %% random:seed(A, B, C),
%%      %% erl18 新写法
%%      rand:seed(exs1024),
%%      put(gel_random_seed, 1);
%%    _ -> skip
%%  end,
  M = Min - 1,
  M + rand:uniform(Max - M).


%% 随机字符串用到的可见字符，选择32个不容易混淆的
-define(MUD_RAND_STRING, {
  $P, $A, $N, $G, $B, $1, $U, $8, $D, $7, $K, $5, $F, $9, $H, $3,
  $L, $X, $M, $6, $Q, $C, $S, $2, $T, $4, $V, $0, $R, $W, $E, $Y
}).  % 0-9+字母，少了IJOZ，并打乱了顺序

%% 生成固定位数的字符串
random_string(N) ->
  Bytes = N * 5 div 8 + 1,
  Bin = crypto:strong_rand_bytes(Bytes),
  random_string(N, Bin, []).
random_string(0, _Bin, Ret) -> Ret;
random_string(N, Bin, Ret) ->
  <<Pos:5, RestBin/bitstring>> = Bin,
  random_string(N - 1, RestBin, [element(Pos + 1, ?MUD_RAND_STRING) | Ret]).













