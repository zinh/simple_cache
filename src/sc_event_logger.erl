-module(sc_event_logger).
-behaviour(gen_event).

-export([add_handler/0, delete_handler/0]).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

add_handler() ->
  sc_event:add_handler(?MODULE, []).

delete_handler() ->
  sc_event:delete_handler(?MODULE, []).

init([]) ->
  {ok, #state{}}.

handle_event({lookup, Key}, State) ->
  io:fwrite("[INFO] Lookup for key ~p~n", [Key]),
  {ok, State};

handle_event({create, {Key, Value}}, State) ->
  io:fwrite("[INFO] New key create: ~p:~p~n", [Key, Value]),
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
