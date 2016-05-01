-module(gws_connection_sup).
-behaviour(supervisor).
-export([start_link/4, start_child/1]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link(Callback, IP, Port, UserArgs) ->
  {ok, Pid} = supervisor:start_link(?MODULE, [Callback, IP, Port, UserArgs]),
  start_child(Pid),
  {ok, Pid}.

start_child(Server) ->
  supervisor:start_child(Server, []).

init([Callback, IP, Port, UserArgs]) ->
  BasicSockOps = [binary, 
    {active, false}, 
    {packet, http_bin}, 
    {reuseaddr, true}],
  SockOps = case IP of
    undefined -> BasicSockOps;
    _Other -> [{ip, IP} | BasicSockOps]
  end,
  io:format("Socket starting~n"),
  {ok, LSock} = gen_tcp:listen(Port, SockOps),
  io:format("Socket started~n"),
  Server = {gws_server, {gws_server, start_link, [Callback, LSock, UserArgs]}, temporary, brutal_kill, worker, [gws_server]},
  RestartStategry = {simple_one_for_one, 0, 1},
  {ok, {RestartStategry, [Server]}}.
