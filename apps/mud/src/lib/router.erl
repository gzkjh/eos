%%%-------------------------------------------------------------------
%%% @author Commando
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 八月 2019 15:28
%%%-------------------------------------------------------------------
-module(router).
-author("Commando").

-include("public.hrl").
-include("all_pb.hrl").

%% API
-export([unpack/1, pack/1]).


unpack(Bin) ->
  %% 先把协议头解出来
  ProtoID = all_pb:decode_msg(Bin, protoID),
  Pt = ProtoID#protoID.proto_id,
  %% 查找协议信息
  case ets:lookup(ets_user_action, Pt) of
    [Pt, {Pt, Atom, Mods}] ->
      Data = all_pb:decode_msg(ProtoID#protoID.data, Atom),
      {Pt, Data, Mods};
    _ -> {error, ?BS("没有相关协议")}
  end.

pack(Data) when is_tuple(Data) ->
  Rec = element(1, Data),
  case ets:lookup(ets_user_action, Rec) of
    [{Rec, {Pt, Rec, _Mods}}] ->
      Bin = all_pb:encode_msg(Data),
      Data2 = #protoID{proto_id = Pt, data = Bin},
      all_pb:encode_msg(Data2);
    _ ->
      ?error("cann't pack ~p", [Data]),
      <<>>
  end.







