-module(gen_web_server).

%% API exports
-export([behaviour_info/1, start_link/3, start_link/4, http_reply/3, http_reply/2, http_reply/1]).
-export([headers/1]).

%%====================================================================
%% API functions
%%====================================================================
behaviour_info(callbacks) ->
  [{init, 1},
    {head, 3},
    {get, 3},
    {delete, 3},
    {options, 4},
    {post, 4},
    {put, 4},
    {trace, 4},
    {other_methods, 4}];

behaviour_info(_Other) ->
  undefined.

%% API
start_link(Callback, Port, UserArgs) ->
  start_link(Callback, undefined, Port, UserArgs).

start_link(Callback, IP, Port, UserArgs) ->
  gws_connection_sup:start_link(Callback, IP, Port, UserArgs).

http_reply(Code, Headers, Body) ->
  ContentBin = iolist_to_binary(Body),
  Length = byte_size(ContentBin),
  [io_lib:format("HTTP/1.1 ~s\r\n~sContent-Length: ~w\r\n\r\n", [response(Code), headers(Headers), Length]), ContentBin].

http_reply(Code) ->
  http_reply(Code, <<>>).

http_reply(Code, Body) ->
  http_reply(Code, [{"Content-Type", "text/html"}], Body).

headers([{Key, Val} | Headers]) ->
  [io_lib:format("~s: ~s\r\n", [Key, Val]) | headers(Headers)];
headers([]) ->
  [].

response(200) ->
  "200 OK";

response(404) ->
  "404 Not Found".
