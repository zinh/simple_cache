-module(resource_discovery).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    add_target_resource_type/1,
    add_local_resource/2,
    fetch_resources/1,
    trade_resources/0
  ]).

%% Callback
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
  ]).

-define(SERVER, ?MODULE).

-record(state, {target_resource_types, %% I want
    local_resources_types, %% I have
    found_resource_types}). %% Instance matching wanted list

%% API
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_target_resource_type(Type) ->
  gen_server:cast(?SERVER, {add_target, Type}).

add_local_resource(Type, Instance) ->
  gen_server:cast(?SERVER, {add_local_resource, {Type, Instance}}).

trade_resources() ->
  gen_server:cast(?SERVER, trade_resources).

fetch_resources(Type) ->
  gen_server:call(?SERVER, {fetch_resources, Type}).


%% Callback
init([]) ->
  {ok, #state{target_resource_types = [], local_resources_types = dict:new(), found_resource_types = dict:new()}}.

handle_cast({add_target, Type}, #state{target_resource_types = Target} = State) ->
  NewList = [Type | lists:delete(Type, Target)],
  {noreply, State#state{target_resource_types = NewList}};

handle_cast({add_local_resource, {Type, Instance}}, State) ->
    LocalResources = State#state.local_resources_types,
    {noreply, State#state{local_resources_types = add_resource(Type, Instance, LocalResources)}};

handle_cast(trade_resources, State) ->
  AllNodes = [node() | nodes()],
  LocalResources = State#state.local_resources_types,
  lists:foreach(
    fun(Node) ->
        gen_server:cast({?SERVER, Node},
          {trade_resources, {node(), LocalResources}})
    end,
    AllNodes),
  {noreply, State};

handle_cast({trade_resources, {ReplyTo, Remotes}},
  #state{target_resource_types = Targets,
    local_resources_types = Locals,
    found_resource_types = Founds } = State) ->
  FilteredRemotes = resource_for_types(Targets, Remotes),
  NewFounds = add_resources(FilteredRemotes, Founds),
  case ReplyTo of
    noreply ->
      ok;
    _ ->
      gen_server:cast({?SERVER, ReplyTo}, {trade_resources, {noreply, Locals}})
  end,
  {noreply, State#state{found_resource_types = NewFounds}}.

handle_call({fetch_resources, Type}, _From, State) ->
  {reply, dict:find(Type, State#state.found_resource_types), State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Private
add_resource(Type, Resource, ResourceTuples) ->
  case dict:find(Type, ResourceTuples) of
    {ok, Resources} ->
      NewList = [Resource | lists:delete(Resource, Resources)],
      dict:store(Type, NewList, ResourceTuples);
    error ->
      dict:store(Type, [Resource], ResourceTuples)
  end.

resource_for_types(TargetTypes, RemoteTuples) ->
  Fun = 
  fun(Type, Acc) ->
      case dict:find(Type, RemoteTuples) of
        {ok, List} ->
          [{Type, Instance} || Instance <- List] ++ Acc;
        error ->
          Acc
      end
  end,
  lists:foldl(Fun, [], Types).
