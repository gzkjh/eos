%% -*- coding: utf-8 -*-
%% Automatically generated, do not edit
%% Generated by gpb_compile version 4.10.0

-ifndef(msg_pb).
-define(msg_pb, true).

-define(msg_pb_gpb_version, "4.10.0").

-ifndef('MSGTEST1_PB_H').
-define('MSGTEST1_PB_H', true).
-record(msgTest1,
        {ii = 0                 :: integer() | undefined, % = 1, 32 bits
         ss = <<>>              :: iodata() | undefined % = 2
        }).
-endif.

-ifndef('MSGTEST2_PB_H').
-define('MSGTEST2_PB_H', true).
-record(msgTest2,
        {ii = 0                 :: integer() | undefined, % = 1, 32 bits
         ss = <<>>              :: iodata() | undefined % = 2
        }).
-endif.

-endif.
