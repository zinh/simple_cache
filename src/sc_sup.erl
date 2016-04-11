%%%-------------------------------------------------------------------
%% @doc simple_cache top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Value, LeaseTime) ->
  supervisor:start_child(?SERVER, [Value, LeaseTime]).
%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  Element = {sc_element, {sc_element, start_link, []},
             temporary, brutal_kill, worker, [sc_element]},
  Child = [Element],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Child} }.

%%====================================================================
%% Internal functions
%%====================================================================
