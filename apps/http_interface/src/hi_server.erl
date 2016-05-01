-module(hi_server).
-behaviour(gen_web_server).

-export([start_link/1, init/1]).
-export([get/3]).

start_link(Port) ->
  gen_web_server:start_link(?MODULE, Port, []).

%% Callback
init([]) ->
  {ok, []}.

get("/test", _Body, _UserData) ->
  Reply = gen_web_server:http_reply(200, "Testing 1 2 3\n"),
  {reply, Reply, []};

get(_Path, _Body, _UserData) ->
  Reply = gen_web_server:http_reply(404),
  {reply, Reply, []}.

put(_Path, _Body, _UserData) ->
  Reply = gen_web_server:http_reply(200, "Hello world"),
  {reply, Reply, []}.
