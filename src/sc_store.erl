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
  case mnesia:dirty_read(sc_store, Key) of
    [#sc_store{pid=Pid}] ->
      {ok, Pid};
    _ ->
      {error, not_found}
  end.

delete(Pid) ->
  mnesia:transaction(fun() ->
                         {atomic, Ids} = mnesia:select(sc_store, [#sc_store{key='$1'}, pid=Pid], [], ['$1']),
                         lists:foreach(fun(Id) -> 
                                           mnesia:delete({sc_store, Id})
                                       end, Ids)
                     end
                    ).

create_tables() ->
  mnesia:create_table(sc_store, [{attributes, record_info(fields, sc_store)}, {index, [pid]}]).
