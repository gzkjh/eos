%%%-------------------------------------------------------------------
%%% @author Commando
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 七月 2019 17:17
%%%-------------------------------------------------------------------
-module(t2).
-author("Commando").



%% gen_server callbacks
-export([handle_call/4, handle_cast/3, handle_info/3]).



handle_call(Request, _From, Continue, State) ->
  io:format("t2 call ~p ~p ~p~n", [Request, Continue, State]),
  {continue, "t2 continue", "t2_call_state"}.

handle_cast(Request, Continue, State) ->
  io:format("t2 cast ~p ~p ~p~n", [Request, Continue, State]),
  {noreply, "t2_cast_state"}.

handle_info(Request, Continue, State) ->
  io:format("t2 info ~p ~p ~p~n", [Request, Continue, State]),
  {continue, "t2 continue", "t2_info_state"}.



