%%%-------------------------------------------------------------------
%% @doc ghostlight public API
%% @end
%%%-------------------------------------------------------------------

-module(ghostlight_app).

-behaviour(application).
%-compile([{compile_transform, lager_transform}]).
%% Application callbacks
-export([start/2, stop/1]).  

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    lager:start(),
    lager:info("HELLO GHOSTLIGHT"),
    initiate_listening_to_endpoints(),
    ghostlight_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================


initiate_listening_to_endpoints() ->
    Port = 8080,
    Dispatch = cowboy_router:compile([
                                      {'_', [
                                             {"/shows", ghostlight_show, []}
                                      ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
                                                            {env, [{dispatch, Dispatch}]}
    ]),
    lager:info("Started the Cowboy server on Port ~p~n", [Port]).

