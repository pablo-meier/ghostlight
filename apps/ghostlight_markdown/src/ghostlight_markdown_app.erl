-module(ghostlight_markdown_app).
-behaviour(application).
-export([start/2,
         stop/1]).

start(_StartType, _StartArgs) ->
    ghostlight_markdown_sup:start_link().

stop(_State) ->
    ok.

