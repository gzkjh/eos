%% -*- coding: utf-8 -*-
%% Automatically generated, do not edit
%% Generated by gpb_compile version 4.10.0

-ifndef(cmd_pb).
-define(cmd_pb, true).

-define(cmd_pb_gpb_version, "4.10.0").

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

-endif.