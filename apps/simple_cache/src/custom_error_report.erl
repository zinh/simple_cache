-module(custom_error_report).
-behaviour(gen_event).

-export([register_with_logger/0]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

register_with_logger() ->
  error_logger:add_report_handler(?MODULE).

init([]) ->
  {ok, #state{}}.

handle_event({info_msg, _Gleader, {Pid, Format, Data}}, State) ->
  io:fwrite("[INFO] <~p> ~s", [Pid, io_lib:format(Format, Data)]),
  {ok, State}.

handle_call(_Request, State) ->
  Reply = ok,
  {ok, Reply, State}.

handle_info(_Info, State) ->
  {ok, State}.

terminate(_Args, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
