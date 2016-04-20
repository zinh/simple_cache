-module(sc_store).
-export([lookup/1, insert/2, init/0, delete/1]).

-define(DEFAULT_LEASE_TIME, 5000).

-record(sc_store, {key, pid}).

init() ->
  mnesia:start(),
  create_tables(),
  ok.

insert(Key, Pid) ->
  mnesia:dirty_write(sc_store, #sc_store{key=Key, pid=Pid}).

lookup(Key) ->
  case mnesia:read(sc_store, Key) of
    [#{pid=Pid}] ->
      {ok, Pid};
    _ ->
      {error, not_found}
  end.

delete(Pid) ->
  ets:match_delete(?TABLE_ID, {'_', Pid}).

create_tables() ->
  mnesia:create_table(sc_store, [{attributes, record_info(fields, sc_store)}]).
