%%%-------------------------------------------------------------------
%%% @author Commando
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 基于cowboy的http模块
%%%
%%% @end
%%% Created : 29. 七月 2019 16:58
%%%-------------------------------------------------------------------
-module(gateway_http).
-author("Commando").

-include("public.hrl").


%% ============================================================================

-export([init/2, start_link/1]).

%% 启动函数
start_link(Opt) ->
  Port = maps:get(port, Opt, 0), %% 端口，默认0
  % 状态
  State = #{},
  WebSocket = cowboy_router:compile([
    %% HostMatch, list({PathMatch, Handler, InitialState})
    {'_', [
      {"/", ?MODULE, State}
    ]}
  ]),
  GamePort = Port,
  % Name, TransOpts, PortoOpts
  {ok, _} = cowboy:start_clear(gateway_http,
    [{port, GamePort}],
    #{env => #{dispatch => WebSocket}}
  ),
  ?notice("~p listen on ~p", [?MODULE, Port]),
  % 自身不启动，返回ignore
  ?ignore.


%% ================================================================================================

%% 初始化，连接
init(Req, State) ->
  Method = cowboy_req:method(Req),
  Reply = handle(Method, Req),
  {ok, Reply, State}.

handle(<<"GET">>, Req) ->
  ReplyBody = <<"handle get ok!">>,
  cowboy_req:reply(200, #{
    <<"content-type">> => <<"text/plain; charset=utf-8">>
  }, ReplyBody, Req);
handle(<<"POST">>, Req0) ->
  HasBody = cowboy_req:has_body(Req0),
  {ok, Body, Req} =
    if
      HasBody =:= ?true ->
        cowboy_req:read_urlencoded_body(Req0);
      HasBody =:= ?false ->
        {ok, <<>>, Req0}
    end,
  {ok, Body, Req} = cowboy_req:read_urlencoded_body(Req0),
  ReplyBody = <<"handle post ok!">>,
  cowboy_req:reply(200, #{
    <<"content-type">> => <<"text/plain; charset=utf-8">>
  }, ReplyBody, Req);
handle(_, Req) ->
  %% Method not allowed.
  cowboy_req:reply(405, Req).
