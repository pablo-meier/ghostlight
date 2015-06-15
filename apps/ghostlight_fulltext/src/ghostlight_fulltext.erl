%%%-------------------------------------------------------------------
%%% @author pablo
%%% @copyright (C) 2015, pablo
%%% @doc
%%% Primary functionality of the full-text search. Uses Elastic as a backend.
%%% @end
%%% Created : 2015-06-15 00:09:38.668156
%%%-------------------------------------------------------------------
-module(ghostlight_fulltext).

-behaviour(gen_server).

%% API
-export([start_link/0,
         find/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
%    Host = ghostlight_config:get('PGHOST'),
%    User = ghostlight_config:get('PGUSER'),
%    Password = ghostlight_config:get('PGPASSWORD'),
%    Database = ghostlight_config:get('PGDATABASE'),

    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% API
%%%===================================================================


%%%-------------------------------------------------------------------
%%% @doc
%%% Primary functionality of the full-text search. Uses Elastic as a backend.
%%% @end
%%%-------------------------------------------------------------------

find(_TextFragment) ->
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================


