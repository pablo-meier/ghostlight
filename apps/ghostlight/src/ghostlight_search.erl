%%%-------------------------------------------------------------------
%%% @author pablo
%%% @copyright (C) 2015, pablo
%%% @doc
%%% HTML-only endpoint (for now?) for a user to see all their search
%%% results get hints back for what is there.
%%% @end
%%% Created : 2015-06-23 00:09:38.668156
%%%-------------------------------------------------------------------
-module(ghostlight_search).

-export([init/2,
         terminate/3]).

init(Req, _Opts) ->
  {ok, Req, []}.

terminate(_Reason, _Req, _State) ->
  ok.
