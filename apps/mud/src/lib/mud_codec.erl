%%%-------------------------------------------------------------------
%%% @author Commando
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. 八月 2019 11:15
%%%-------------------------------------------------------------------
-module(mud_codec).
-author("Commando").

%% API
-export([]).

-callback encode(Term :: term()) -> byte().
-callback decode(Data :: byte(), Term :: term()) -> term().
