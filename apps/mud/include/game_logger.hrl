-ifndef(TRACEX).
-define(TRACEX(X), io:format("~w:~w ====> ~n~p ~n", [?MODULE, ?LINE, X])).
-endif.

-ifndef(PRINT).
-define(PRINT(X), io:format("~w:~w ====> ~w ~n", [?MODULE, ?LINE, X])).
-endif.

-ifndef(INFO).
-define(INFO(Args), lager:info(Args)).
-define(INFO(Format, Args), lager:info(Format, Args)).
-endif.

-ifndef(WARNING).
-define(WARNING(Format, Args), game_logger:warning(?MODULE, ?LINE, Format, Args)).
-define(WARNING(Msg), game_logger:warning(?MODULE, ?LINE, Msg, [])).
-endif.

-ifndef(FOOTMARK).
-define(FOOTMARK, game_logger:footmark(?MODULE, ?LINE, ok)).
-define(FOOTMARK(Args), game_logger:footmark(?MODULE, ?LINE, Args)).
-endif.



