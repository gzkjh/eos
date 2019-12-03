-include("game_logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl"). % 使用 ets:fun2ms/1 必须要的


-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, true}, {delay_send, false}, {send_timeout, 5000}, {keepalive, false}, {exit_on_close, true}]).
-define(TCP_OPTIONS1, [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, true}, {delay_send, false}, {send_timeout, 5000}, {keepalive, false}, {exit_on_close, true}]).

-define(DIFF_SECONDS_1970_1900, 2208988800).
-define(DIFF_SECONDS_0000_1900, 62167219200).
-define(ONE_DAY_SECONDS, 86400).              %%一天的时间（秒）
-define(ONE_DAY_MILLISECONDS, 86400000).  %%一天时间（毫秒）

-ifndef(TRY).
-define(TRY(B, T), (try (B) catch _:_ -> (T) end)).
-endif.
-ifndef(IF).
-define(IF(B, T, F), (case (B) of true -> (T); false -> (F) end)).
-endif.




