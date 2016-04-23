-module(sc_store_ets).
-export([lookup/1, insert/2, init/0, delete/1]).

-define(DEFAULT_LEASE_TIME, 5000).
-define(TABLE_ID, ?MODULE).

init() ->
  ets:new(?TABLE_ID, [public, named_table]),
  ok.

insert(Key, Pid) ->
  ets:insert(?TABLE_ID, {Key, Pid}).

lookup(Key) ->
  case ets:lookup(?TABLE_ID, Key) of
    [ {Key, Pid} | _ ] ->
      {ok, Pid};
    _ ->
      {error, not_found}
  end.

delete(Pid) ->
  ets:match_delete(?TABLE_ID, {'_', Pid}).
