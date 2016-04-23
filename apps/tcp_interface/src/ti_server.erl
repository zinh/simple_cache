-module(ti_server).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% Callback
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).

-define(SERVER, ?MODULE).
-record(state, {lsock}).

%% API
start_link(LSock) ->
  gen_server:start_link(?SERVER, [LSock], []).

%% Callback
init([LSock]) ->
  {ok, #state{lsock=LSock}, 0}.

handle_call(_Req, _From, State) ->
  {noreply, State}.

handle_cast(_Req, State) ->
  {noreply, State}.

handle_info(timeout, #state{lsock=LSock} = State) ->
  {ok, _Socket} = gen_tcp:accept(LSock),
  tcp_interface_sup:start_child(),
  {noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
  {stop, normal, State};

handle_info({tcp, Socket, RawData}, State) ->
  reply(RawData, Socket),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Private function
reply(RawData, Socket) ->
  {Op, Params} = parse_fa(RawData),
  case Op of
    "insert" ->
      [Key | Value] = Params,
      case apply(simple_cache, insert, [Key, Value]) of
        ok ->
          gen_tcp:send(Socket, io_lib:fwrite("OK~n", []));
        _Other ->
          gen_tcp:send(Socket, io_lib:fwrite("ERROR~n", []))
      end;
    "lookup" ->
      case apply(simple_cache, lookup, Params) of
        failed -> gen_tcp:send(Socket, io_lib:fwrite("ERROR~n", []));
        Result -> gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Result]))
      end;
    _Other ->
      gen_tcp:send(Socket, "Unsupported operator")
  end.



%% insert(key, val)
%% lookup(key)
%%   -> OK:{key, val}
%%   -> ERROR: msg
parse_fa(RawData) ->
  Data = re:replace(RawData, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
  [Op | Params] = re:split(Data, "[(),]", [{return, list}, trim]),
  {Op, Params}.
