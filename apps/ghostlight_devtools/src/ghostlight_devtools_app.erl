%%%-------------------------------------------------------------------
%% @doc ghostlight_devtools public API
%% @end
%%%-------------------------------------------------------------------

-module(ghostlight_devtools_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1]).

start(_StartType, _StartArgs) ->
    ghostlight_devtools_sup:start_link().

stop(_State) ->
    ok.

