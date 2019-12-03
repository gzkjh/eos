%%%-------------------------------------------------------------------
%%% @author Commando
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 八月 2019 14:08
%%%-------------------------------------------------------------------
-module(client).
-author("Commando").

-include("public.hrl").
-include("all_pb.hrl").
%% API
-export([send/0]).


send() ->
  Bin1 = codec_pbf:encode(#cmdTest1{ii = 11, ss = "abc"}),
  Bin2 = codec_pbf:encode(#cmdTest2{ii = 22, ss = "def"}),
  {ok, Socket} = gen_tcp:connect("127.0.0.1", 20001, []),
  ?notice("~p ~p", [Bin1, Bin2]),
  Len1 = byte_size(Bin1),
  Len2 = byte_size(Bin2),
  ok = gen_tcp:send(Socket, <<Len1:16, Bin1/bytes>>),
  ok = gen_tcp:send(Socket, <<Len2:16, Bin2/bytes>>),
  ok.
