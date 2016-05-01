-module(hi_server).
-behaviour(gen_web_server).

-export([start_link/1]).

start_link(Port) ->
  gen_web_server:start_link(?MODULE, Port, []).

%% Callback
init([]) ->
  {ok, []}.

get("/test", _Body, _UserData) ->
  gen_web_server:http_reply(200, "Hello world").
