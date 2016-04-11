-module(sc_element).
-behaviour(gen_server).

-define(DEFAULT_LEASE_TIME, (60*60*25)).
-define(SERVER, ?MODULE).

-record(state, {value, lease_time, start_time}).

start_link(Value, LeaseTime) ->
  gen_server:start_link(?SERVER, [Value, LeaseTime], []).

init([Value, LeaseTime]) ->
  Now = calendar:local_time(),
  StartTime = calendar:datetime_to_gregorian_seconds(Now),
  {ok, #state{value = Value,
              lease_time = LeaseTime,
              start_time = StartTime}}.

%% time_left(LeaseTime, StartTime) ->
