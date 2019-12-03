%%%-------------------------------------------------------------------
%%% @author Commando
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 连接器的实现，主要是负责数据交换
%%% 目前这个linker是配合 ranch 使用的
%%% 工作流程：
%%% 1、接受新的连接，会生成该进程，同时可以指定编码器模块
%%% 2、转发所有信息给，login进程（暂时固定名字，以后考虑也可以动态配置）
%%%
%%%
%%%
%%%
%%%
%%%
%%% @end
%%% Created : 31. 七月 2019 15:20
%%%-------------------------------------------------------------------
-module(linker_ranch_tcp).
-author("Commando").
-include("public.hrl").

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3, start_link/4]).

%% 数据包头大小（单位：位）
-define(HEAD_BIT, 16).
%% 数据包头大小（单位：字节）
-define(HEAD_BYTE, 2).

%% socket参数
-define(TCP_OPTIONS, [
  {packet, ?HEAD_BYTE},  %% 包头长度（单位：字节）
  {active, 100},  %% 每次最大处理包数量
  {nodelay, true},
  {keepalive, true},
  {exit_on_close, true},
  {send_timeout, 8000},
  {send_timeout_close, true}
]).

-record(state, {
  %% 通讯socket
  socket = ?undefined,
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

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
%%-spec(start_link() ->
%%  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Ref, _Socket, Transport, TransOptions) ->
  {ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Transport, TransOptions}])}.

init({Ref, Transport, TransOptions}) ->
  ?notice("linker init ~p", [TransOptions]),
  ModCodec = maps:get(mod_codec, TransOptions, ?undefined),

  %% Perform any required state initialization here.
  {ok, Socket} = ranch:handshake(Ref),
  ok = Transport:setopts(Socket, ?TCP_OPTIONS),
  State = #state{
    socket = Socket,
    codec = ModCodec
  },
  gen_server:enter_loop(?MODULE, [], State).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
%%-spec(init(Args :: term()) ->
%%  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
%%  {stop, Reason :: term()} | ignore).
%%init([Ref, Transport] = A) ->
%%  ?notice("linker init ~p", [A]),
%%  {ok, Socket} = ranch:handshake(Ref),
%%  ok = Transport:setopts(Socket, [{active, true}]),
%%  gen_server:enter_loop(?MODULE, [], #{}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(Request, _From, State) ->
  ?notice("linker call ~p", [Request]),
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(Request, State) ->
  ?notice("linker cast ~p", [Request]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
%% 接收数据
handle_info({tcp, _Socket, Data}, #state{codec = Codec} = State) ->
  %% todo 解密
  %% 解码
  Data2 =
    case State#state.codec of
      ?undefined -> Data;
      Codec -> Codec:decode(Data)
    end,
  %% 转交
  hand_over(Data2, State),
  {noreply, State};

%% 发送数据
handle_info({send2client, Bin}, #state{socket = Socket} = State) ->
  %% todo 加密
  Len = byte_size(Bin),
  %% 头部加2字节长度
  gen_tcp:send(Socket, <<Len:?HEAD_BIT, Bin/bytes>>),
  {noreply, State};

handle_info(Info, State) ->
  ?notice("linker info ~p", [Info]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(Reason, _State) ->
  ?debug("linker stop :~p", [Reason]),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% 移交登录进程处理
hand_over(Data, #state{user_pid = ?undefined, login_pid = PidTo}) ->
  PidTo ! {recv, Data};
%% 移交用户进程处理
hand_over(Data, #state{user_pid = PidTo}) when is_pid(PidTo) ->
  PidTo ! {recv, Data};
hand_over(Data, State) ->
  ?error("linker hand over fail ~p ~p", [Data, State]).




