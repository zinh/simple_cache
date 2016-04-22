-module(simple_cache).
-export([insert/2, insert/3, lookup/1]).

insert(Key, Value) ->
  sc_event:create(Key, Value),
  case sc_store:lookup(Key) of
    {ok, Pid} ->
      sc_element:replace(Pid, Value);
    {error, _} ->
      {ok, Pid} = sc_element:create(Value),
      sc_store:insert(Key, Pid)
  end.

insert(Key, Value, LeaseTime) ->
  sc_event:create(Key, Value),
  case sc_store:lookup(Key) of
    {ok, Pid} ->
      sc_element:replace(Pid, Value, LeaseTime);
    {error, _} ->
      {ok, Pid} = sc_element:create(Value, LeaseTime),
      sc_store:insert(Key, Pid)
  end.

lookup(Key) ->
  sc_event:lookup(Key),
  case sc_store:lookup(Key) of
    {ok, Pid} ->
      sc_element:get_value(Pid);
    {error, _} ->
      failed
  end.
