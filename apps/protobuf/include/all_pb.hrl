%% -*- coding: utf-8 -*-
%% Automatically generated, do not edit
%% Generated by gpb_compile version 4.10.0

-ifndef(all_pb).
-define(all_pb, true).

-define(all_pb_gpb_version, "4.10.0").

-ifndef('PROTOID_PB_H').
-define('PROTOID_PB_H', true).
-record(protoID,
        {proto_id = 0           :: integer() | undefined, % = 1, 32 bits
         data = <<>>            :: iodata() | undefined % = 2
        }).
-endif.

-ifndef('CMDTEST1_PB_H').
-define('CMDTEST1_PB_H', true).
-record(cmdTest1,
        {ii = 0                 :: integer() | undefined, % = 2, 32 bits
         ss = <<>>              :: iodata() | undefined % = 3
        }).
-endif.

-ifndef('CMDTEST2_PB_H').
-define('CMDTEST2_PB_H', true).
-record(cmdTest2,
        {ii = 0                 :: integer() | undefined, % = 1, 32 bits
         ss = <<>>              :: iodata() | undefined % = 2
        }).
-endif.

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
