-module(log_simulator).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(SLEEP_TIME, 3000).

start_link() ->
  gen_server:start_link({local, ?SERVER}, log_simulator, [?SLEEP_TIME], []).

init([SleepTime]) ->
  {ok, [], SleepTime}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(timeout, State) ->
  die_here = right_now,
  {stop, timeout, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
