-module(simple_cache).
-export([insert/2, lookup/1]).

insert(Key, Value) ->
  case sc_store:lookup(Key) of
    {ok, Pid} ->
      sc_element:replace(Pid, Value);
    {error, _} ->
      {ok, Pid} = sc_element:create(Value),
      sc_store:insert(Key, Pid)
  end.

lookup(Key) ->
  case sc_store:lookup(Key) of
    {ok, Pid} ->
      sc_element:get_value(Pid);
    {error, _} ->
      failed
  end.
