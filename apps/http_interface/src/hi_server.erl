-module(hi_server).
-behaviour(gen_web_server).

-export([start_link/1]).
-export([init/1, get/3, put/3]).

start_link(Port) ->
  gen_web_server:start_link(?MODULE, Port, []).

%% Callback
init([]) ->
  {ok, []}.

get(Path, _Body, _UserData) ->
  Key = path_to_key(Path),
  Reply = case apply(simple_cache, lookup, [Key]) of
    failed -> gen_web_server:http_reply(404, "Not found\n");
    Result -> gen_web_server:http_reply(200, Result)
  end,
  {reply, Reply, []}.

put(Path, Body, _UserData) ->
  Key = path_to_key(Path),
  Reply = case apply(simple_cache, insert, [Key, Body]) of
    ok -> gen_web_server:http_reply(200, "OK");
    _ -> gen_web_server:http_reply(500, "Failed\n")
  end,
  {reply, Reply, []}.

path_to_key(Path) ->
  Paths = re:split(Path, "[/]", [{return, list}]),
  lists:nth(2, Paths).
