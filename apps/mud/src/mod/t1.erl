%%%-------------------------------------------------------------------
%%% @author Commando
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 七月 2019 17:17
%%%-------------------------------------------------------------------
-module(t1).
-author("Commando").

-behaviour(mud_server).

%% gen_server callbacks
-export([init/1, handle_call/4, handle_cast/3, handle_info/3, start_link/0]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  mud_server:start_link({local, ?MODULE}, ?MODULE, []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #{state=>term(),
  handle_call=>[fun((Request :: term(), From :: term(), Continue :: term(), State :: term())-> term())],
  handle_cast=>[fun((Request :: term(), Continue :: term(), State :: term())-> term())],
  handle_info=>[fun((Request :: term(), Continue :: term(), State :: term())-> term())]}
  }).
init([]) ->


%%  {_, A1, B1} = os:timestamp(),
%%
%%  D1 = dict:new(),
%%  D2 = born_dict(D1, 1000000),
%%  search_dict(D2, 1000000),

%%  D1 = #{},
%%  D2 = born_map(D1, 1000000),
%%  search_map(D2, 1000000),

%%  {_, A2, B2} = os:timestamp(),
%%  C1 = A1 * 1000 + B1 div 1000,
%%  C2 = A2 * 1000 + B2 div 1000,
%%  io:format("run cost : ~p ms~n", [C2 - C1]),

  {ok, #{
    state => "init_state",
    handle_call => [fun handle_call/4, fun t2:handle_call/4, fun t3:handle_call/4],
    handle_cast => [fun handle_cast/3, fun t2:handle_cast/3, fun t3:handle_cast/3],
    handle_info => [fun handle_info/3, fun t2:handle_info/3, fun t3:handle_info/3]
  }}.


handle_call(Request, _From, Continue, State) ->
  io:format("t1 call ~p ~p ~p~n", [Request, Continue, State]),
  {continue, "t1 continue", "t1_call_state"}.


handle_cast(Request, Continue, State) ->
  io:format("t1 cast ~p ~p ~p~n", [Request, Continue, State]),
  {continue, "t1 continue", "t1_cast_state"}.

handle_info(Request, Continue, State) ->
  io:format("t1 info ~p ~p ~p~n", [Request, Continue, State]),
  {continue, "t1 continue", "t1_info_state"}.


%%born_map(Data, 0) -> Data;
%%born_map(Data, Num) ->
%%  born_map(Data#{Num=>Num}, Num - 1).
%%
%%search_map(_Data, 0) -> ok;
%%search_map(Data, Count) ->
%%  Key = rand:uniform(1000000),
%%  maps:get(Key, Data),
%%  search_map(Data, Count - 1).
%%
%%
%%
%%born_dict(Data, 0) -> Data;
%%born_dict(Data, Num) ->
%%  born_dict(dict:store(Num, Num, Data), Num - 1).
%%
%%search_dict(_Data, 0) -> ok;
%%search_dict(Data, Count) ->
%%  Key = rand:uniform(1000000),
%%  dict:find(Key, Data),
%%  search_dict(Data, Count - 1).


