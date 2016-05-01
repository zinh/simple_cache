-module(gws_server).
-behaviour(gen_server).
-export([start_link/3]).
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3, handle_cast/2]).

-record(state, {lsock, socket, request_line, headers=[], body = <<>> , content_remaining=0, callback, user_data, parent}).

start_link(Callback, LSock, UserArgs) ->
  gen_server:start_link(?MODULE, [Callback, LSock, UserArgs, self()], []).

init([Callback, LSock, UserArgs, Parent]) ->
  {ok, UserData} = Callback:init(UserArgs),
  State = #state{lsock = LSock, callback=Callback, user_data=UserData, parent=Parent},
  {ok, State, 0}.

handle_info({http, _Socket, {http_request, Action, Path, _Version}}, State) ->
  {abs_path, AbsPath} = Path,
  inet:setopts(State#state.socket, [{active, once}]),
  {noreply, State#state{request_line={Action, binary_to_list(AbsPath)}}};

handle_info({http, _Socket, {http_header, _Length, Name, _ReservedField, Value}}, State) ->
  inet:setopts(State#state.socket, [{active, once}]),
  {noreply, header(Name, Value, State)};

handle_info({http, _Socket, http_eoh}, #state{content_remaining=0} = State) ->
  {stop, normal, handle_http_request(State)};

handle_info({http, _Socket, http_eoh}, State) ->
  inet:setopts(State#state.socket, [{accept, once}, {packet, raw}]),
  {noreply, State};

handle_info({tcp, _Socket, RawData}, State) when is_binary(RawData) ->
  ContentRemaining = State#state.content_remaining - byte_size(RawData),
  Body = list_to_binary([State#state.body, RawData]),
  NewState = State#state{content_remaining=ContentRemaining, body=Body},
  if ContentRemaining > 0 ->
      inet:setopts(State#state.socket, [{active, once}]),
      {noreply, NewState};
    true ->
      {stop, normal, handle_http_request(NewState)}
  end;

handle_info({tcp_closed, _Socket}, State) ->
  {stop, disconnected, State};

handle_info(timeout, #state{lsock=LSock, parent=Parent} = State) ->
  {ok, Socket} = gen_tcp:accept(LSock),
  gws_connection_sup:start_child(Parent),
  inet:setopts(Socket, [{active, once}]),
  {noreply, State#state{socket=Socket}}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

%% Private
header("Content-Length" = Name, Length, #state{headers = Headers} = State) ->
  ContentLength = State#state.content_remaining - list_to_integer(binary_to_list(Length)),
  State#state{headers = [{Name, Length} | Headers], content_remaining = ContentLength};

header(Name, Value, #state{headers=Headers} = State) ->
  State#state{headers=[{Name, Value} | Headers]}.

handle_http_request(#state{body=Body, 
    request_line={Action, Path}, 
    user_data=UserData,
    callback=Callback} = State) ->
  case dispatch(Action, Path, Body, Callback, UserData) of
    {reply, Reply, NewUserData} ->
      gen_tcp:send(State#state.socket, Reply),
      NewState = State#state{user_data = NewUserData};
    {noreply, NewUserData} ->
      NewState = State#state{user_data = NewUserData}
  end,
  NewState.

dispatch('GET', Path, Body, Callback, UserData) ->
  Callback:get(Path, Body, UserData);

dispatch('PUT', Path, Body, Callback, UserData) ->
  Callback:put(Path, Body, UserData).
