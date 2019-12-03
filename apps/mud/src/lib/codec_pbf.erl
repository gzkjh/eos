%%%-------------------------------------------------------------------
%%% @author Commando
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 八月 2019 11:15
%%%-------------------------------------------------------------------
-module(codec_pbf).
-author("Commando").

-behaviour(mud_codec).

-include("public.hrl").
-include_lib("protobuf/include/all_pb.hrl").

%% API
-export([encode/1, decode/1]).


%% 编码
encode(Term) ->
  Rec = element(1, Term),
  case ets:lookup(ets_user_action, Rec) of
    [{Rec, {Pt, Rec, _Mods}}] ->
      Bin = all_pb:encode_msg(Term),
      Data2 = #protoID{proto_id = Pt, data = Bin},
      all_pb:encode_msg(Data2);
    _ ->
      ?error("cann't encode ~p", [Term]),
      <<>>
  end.


%% 解码
decode(Bin) ->
  %% 先把协议头解出来
  ProtoID = all_pb:decode_msg(Bin, protoID),
  Pt = ProtoID#protoID.proto_id,
  %% 查找协议信息
  case ets:lookup(ets_user_action, Pt) of
    [{Pt, {Pt, Atom, Mods}}] ->
      Data = all_pb:decode_msg(ProtoID#protoID.data, Atom),
      {Pt, Data, Mods};
    Ret ->
      ?error("cann't decode proto:~p binary:~p ret:~p", [Pt, Bin, Ret]),
      {error, ?BS("没有相关协议")}
  end.


