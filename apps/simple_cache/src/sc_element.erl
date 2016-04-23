-module(sc_element).
-behaviour(gen_server).
-export([start_link/2, create/1, create/2, replace/2, get_value/1, replace/3]).
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).

-define(DEFAULT_LEASE_TIME, (60*60*25)).
-define(SERVER, ?MODULE).

-record(state, {value, lease_time, start_time}).

start_link(Value, LeaseTime) ->
  gen_server:start_link(?SERVER, [Value, LeaseTime], []).

create(Value) ->
  supervisor:start_child(sc_element_sup, [Value, ?DEFAULT_LEASE_TIME]).

create(Value, LeaseTime) ->
  supervisor:start_child(sc_element_sup, [Value, LeaseTime]).

replace(Pid, Value) ->
  gen_server:cast(Pid, {replace, Value}).

replace(Pid, Value, LeaseTime) ->
  gen_server:cast(Pid, {replace, Value, LeaseTime}).

get_value(Pid) ->
  gen_server:call(Pid, get).

init([Value, LeaseTime]) ->
  Now = calendar:local_time(),
  StartTime = calendar:datetime_to_gregorian_seconds(Now),
  {ok, #state{value = Value,
      lease_time = LeaseTime,
      start_time = StartTime},
    LeaseTime}.

handle_info(timeout, State) ->
  {stop, timeout, State}.

handle_call(get, _From, #state{value = Value, lease_time = LeaseTime} = State) ->
  {reply, Value, State, LeaseTime}.

handle_cast({replace, Value}, State) ->
  {noreply, State#state{value = Value}, State#state.lease_time};

handle_cast({replace, Value, LeaseTime}, State) ->
  {noreply, State#state{value = Value, lease_time=LeaseTime}, LeaseTime}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  sc_store:delete(self()),
  ok.
