%%%-------------------------------------------------------------------
%% @doc simple_cache public API
%% @end
%%%-------------------------------------------------------------------

-module(sc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  %% ok = ensure_contact(),
  sc_store:init(),
  case sc_sup:start_link() of
    {ok, Pid} -> {ok, Pid};
    Other -> {error, Other}
  end.

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
ensure_contact() ->
  DefaultNodes = ['default@microv'],
  case get_env(simple_cache, contact_nodes, DefaultNodes) of
    [] ->
      {error, no_contact_nodes};
    ContactNodes ->
      ensure_contact(ContactNodes)
  end.

ensure_contact(Nodes) ->
  Answering = [N || N <- Nodes, net_adm:ping(N) =:= pong],
  case Answering of
    [] ->
      {error, no_contact_reachable};
    _ ->
      DefaultTime = 6000,
      WaitTime = get_env(simple_cache, wait_time, DefaultTime),
      wait_for_nodes(length(Answering), WaitTime)
  end.

get_env(Type, Name, DefaultVal) ->
  case application:get_env(Type, Name) of
    {ok, Val} -> Val;
    undefined -> DefaultVal
  end.

wait_for_nodes(_Length, _Time) ->
  ok.
