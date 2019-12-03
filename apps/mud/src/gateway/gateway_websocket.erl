%%%-------------------------------------------------------------------
%%% @author Commando
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 基于cowboy的websocket连接模块
%%% 它本身已经做了类似linker的功能，所以直接写逻辑就可以了
%%% @end
%%% Created : 29. 七月 2019 16:58
%%%-------------------------------------------------------------------
-module(gateway_websocket).
-author("Commando").

-include("public.hrl").

-behaviour(cowboy_websocket).

%% ============================================================================

-export([init/2, websocket_init/1, websocket_info/3, start_link/1, terminate/3, websocket_handle/2]).


-record(state, {
  %% 编码模块
  codec = ?undefined,
  %% 用户进程
  user_pid = ?undefined,
  %% 登录处理进程，暂时写死是login_debug
  login_pid = login_debug,

  %% 登录成功后的回调函数，一般是用来生成用户进程，需要返回{ok, pid()}
  fun_logined = ?undefined,

  open_id = ?undefined,
  access_token = ?undefined,
  refresh_token = ?undefined,
  expire_in = 0,

  %% todo 加解密
  crypto = ?undefined,  % 加解密模块
  s_key = ?undefined, % 密匙
  s_ivec = ?undefined, % 初始量

  uid = ?undefined
}).


start_link(Opt) ->
  Port = maps:get(port, Opt, 0), %% 端口，默认0
  ModCodec = maps:get(mod_codec, Opt, ?undefined),
  State = #state{
    codec = ModCodec
  },

  WebSocket = cowboy_router:compile([
    %% HostMatch, list({PathMatch, Handler, InitialState})
    {'_', [
      {'_', ?MODULE, State}
    ]}
  ]),
  GamePort = Port,
  % Name, TransOpts, PortoOpts
  {ok, _} = cowboy:start_clear(gateway_websocket,
    [{port, GamePort}],
    #{env => #{dispatch => WebSocket}}
  ),
  ?notice("~p listen on ~p", [?MODULE, Port]),
  % 自身不启动，返回ignore
  ?ignore.


%% ================================================================================================

%% 初始化
init(Req, State) ->
  ?notice("init ~p ~p", [Req, State]),
%%  ModCodec = maps:get(mod_codec, TransOptions, ?undefined),
  %% 两分钟没信息就自动断开
  {cowboy_websocket, Req, State, #{idle_timeout => 120000}}.


%% 升级成为websocket之前调用
websocket_init(State) ->
  ?notice("websocket_init ~p", [State]),
  {ok, State}.


%% websocket_handle(InFrame, Req, State) ->
%% {ok, Req, State} |   % 只收不回
%% {ok, Req, State, hibernate} |  % 挂起，暂时想不到哪些情况会有用
%% {reply, OutFrame | [OutFrame], Req, State} | % 马上回复的
%% {reply, OutFrame | [OutFrame], Req, State, hibernate} |  % 回复后挂起
%% {shutdown, Req, State} % 断开

%% websocket接受到的信息处理
websocket_handle({text, Text}, State) -> % 未登录
  ?notice("~p receive text ~p", [?MODULE, Text]),
  %% todo 解密
  %% 解码
  Data2 =
    case State#state.codec of
      ?undefined -> Text;
      Codec -> Codec:decode(Text)
    end,
  %% 转交
  hand_over(Data2, State),
  {[], State};
websocket_handle({binary, Bin}, State) ->
%%  ?notice("~p receive binary ~p", [?MODULE, Bin]),
  %% todo 解密
  %% 解码
  Data2 =
    case State#state.codec of
      ?undefined -> Bin;
      Codec -> Codec:decode(Bin)
    end,
  %% 转交
  hand_over(Data2, State),
  {[], State};
websocket_handle(What, State) ->
  ?warning("~p receive unhandle ~p", [?MODULE, What]),
  {[], State}.


%% websocket_info(Info, Req, State) ->
%% {ok, Req, State} |
%% {ok, Req, State, hibernate} |
%% {reply, OutFrame | [OutFrame], Req, State} |
%% {reply, OutFrame | [OutFrame], Req, State, hibernate} |
%% {shutdown, Req, State}

%% 内部信息处理，用作发送信息
websocket_info(Info, Req, State) ->
  ?warning("websocket info:~p", [Info]),
  {ok, Req, State, hibernate}.


%% 终止
terminate(_Reason, _Req, _State) ->
  ok.


%% 移交登录进程处理
hand_over(Data, #state{user_pid = ?undefined, login_pid = PidTo}) ->
  PidTo ! {recv, Data};
%% 移交用户进程处理
hand_over(Data, #state{user_pid = PidTo}) when is_pid(PidTo) ->
  PidTo ! {recv, Data};
hand_over(Data, State) ->
  ?error("websocket hand over fail ~p ~p", [Data, State]).


