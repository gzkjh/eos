%% 这里是放全局的 define

% 游戏监听进程数量
-ifndef(LISTEN_PROCESS_COUNT).
-define(LISTEN_PROCESS_COUNT, 3).
-endif.

% 游戏连接监听端口属性
-ifndef(GAME_LISTEN_TCP_OPTIONS).
-define(GAME_LISTEN_TCP_OPTIONS,
  [
    binary,
    {packet, 2},
    {active, false},
    {reuseaddr, true},
    {nodelay, true},
    {delay_send, false},
    {send_timeout, 5000},
    {keepalive, false},
    {exit_on_close, true}
  ]).
-endif.

% 游戏连接监听端口属性
-ifndef(WEBSOCKET_LISTEN_HTTP_OPTIONS).
-define(WEBSOCKET_LISTEN_HTTP_OPTIONS,
  [
    binary,
    {packet, raw},
    {active, true},
    {reuseaddr, true},
    {nodelay, true},
    {delay_send, false},
    {send_timeout, 2000},
    {keepalive, false},
    {exit_on_close, true}
  ]).
-endif.

% 游戏连接监听端口属性
-ifndef(WEBSOCKET_LINK_OPTIONS).
-define(WEBSOCKET_LINK_OPTIONS,
  [
    binary,
    {packet, raw},
    {active, true},
    {reuseaddr, true},
    {nodelay, true},
    {delay_send, false},
    {send_timeout, 5000},
    {keepalive, true},
    {exit_on_close, true}
  ]).
-endif.

-ifndef(POOL).
-define(POOL_USER, pool_user).
-define(POOL_RANK, pool_rank).
-define(POOL_RECHARGE, pool_recharge).
-define(POOL, true).
-endif.

-ifndef(IF).
-define(IF(BBB, TTT, FFF), (case (BBB) of true -> (TTT); false -> (FFF) end)).
-endif.

-ifndef(LOOP_IF).
-define(LOOP_IF(CurFun_define, ErrCode_define, Reason_define),
  (?LOOP_IF(true, CurFun_define, ErrCode_define, Reason_define))).
-define(LOOP_IF(PRE_Flag_define, CurFun_define, ErrCode_define, Reason_define), (
    case (PRE_Flag_define) of
      true -> case (CurFun_define) of
                true -> true;
                false -> {false, ErrCode_define, Reason_define}
              end;
      _ -> PRE_Flag_define
    end
)).
-endif.


-ifndef(gets).
-define(gets(Fmt), (#{save := Fmt})).
-endif.

-ifndef(EXEC_INFO).
-define(EXEC_INFO(PPPid, MMMoudle, FFFunc, AAArgs),
  (case PPPid of
     ?undefined -> ok;
     _ -> PPPid ! {exec, MMMoudle, FFFunc, AAArgs}
   end)
).
-define(EXEC_INFO(PPPid, FFFunc, AAArgs), (PPPid ! {exec, FFFunc, AAArgs})).
-endif.

%% 时间宏
-ifndef(WEEK_SECOND).
-define(WEEK_SECOND, 604800).
-endif.
-ifndef(DAY_SECOND).
-define(DAY_SECOND, 86400).
-endif.
-ifndef(HOUR_SECOND).
-define(HOUR_SECOND, 3600).
-endif.

%% 中文宏
-ifndef(BS).
-define(BS(S), (<<S/utf8>>)).
-endif.

%% 这些宏是为了方便ide能智能提醒
-ifndef(true).
-define(true, true).
-endif.
-ifndef(false).
-define(false, false).
-endif.
-ifndef(undefined).
-define(undefined, undefined).
-endif.
-ifndef(indefinitely).
-define(indefinitely, indefinitely).
-endif.
-ifndef(continue).
-define(continue, continue).
-endif.
-ifndef(ignore).
-define(ignore, ignore).
-endif.


-ifndef(male).
-define(male, 1).
-endif.
-ifndef(female).
-define(female, 0).
-endif.

%% 信息
-ifndef(critical).
-define(critical(MACROmsg), (lager:critical(MACROmsg))).
-define(critical(MACROfmt, MACROarg), (lager:critical(MACROfmt, MACROarg))).
-endif.
-ifndef(error).
-define(error(MACROmsg), (lager:error(MACROmsg))).
-define(error(MACROfmt, MACROarg), (lager:error(MACROfmt, MACROarg))).
-endif.
-ifndef(warning).
-define(warning(MACROmsg), (lager:warning(MACROmsg))).
-define(warning(MACROfmt, MACROarg), (lager:warning(MACROfmt, MACROarg))).
-endif.
-ifndef(notice).
-define(notice(MACROmsg), (lager:notice(MACROmsg))).
-define(notice(MACROfmt, MACROarg), (lager:notice(MACROfmt, MACROarg))).
-endif.
-ifndef(debug).
-define(debug(MACROmsg), (lager:debug(MACROmsg))).
-define(debug(MACROfmt, MACROarg), (lager:debug(MACROfmt, MACROarg))).
-endif.
-ifndef(footmark).
-define(footmark, (lager:notice("FOOTMARK >> ~p line:~p", [?MODULE, ?LINE]))).
-endif.


%% ets相关的宏
-ifndef(ets_define).

-define(ets_settings, ets_settings).
-define(ets_online_user, ets_online_user).
-define(ets_unlegal_words, ets_unlegal_words).
-define(ets_surname, ets_surname).
-define(ets_name_male, ets_name_male).
-define(ets_name_female, ets_name_female).
-define(ets_wordbook, ets_wordbook).

-define(ets_define, true).
-endif.













