%%%-------------------------------------------------------------------
%%% @author pablo
%%% @copyright (C) 2015, pablo
%%% @doc
%%% JSON-only endpoint for the browser to send little fragments to and
%%% get hints back for what is there.
%%% @end
%%% Created : 2015-06-23 00:09:38.668156
%%%-------------------------------------------------------------------
-module(ghostlight_search_hint).

-export([init/2,
         terminate/3]).

init(Req, _Opts) ->
    {ok, RequestBody, Req2} = cowboy_req:body(Req),
    Req3 = cowboy_req:set_meta(response_type, json, Req2),
    lager:info("RequestBody is ~p~n", [RequestBody]),
    AsJson = jsx:decode(RequestBody, [return_maps]),
    Query = maps:get(<<"query">>, AsJson),
    lager:info("Query is ~p~n", [Query]),
    Response = ghostlight_fulltext:find(Query),
    lager:info("Response is ~p~n", [Response]),
 
    Req4 = cowboy_req:reply(200,
                            [{<<"content-type">>, <<"application/json">>}],
                            Response,
                            Req3),
    {ok, Req4, []}.

terminate(_Reason, _Req, _State) ->
  ok.

