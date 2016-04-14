-module(log_simulator2).
-export([go/0]).

-define(SLEEP_TIME, 3000).

go() ->
  timer:sleep(?SLEEP_TIME),
  die_here = right_now.
