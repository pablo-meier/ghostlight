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
         populate_indices/0,
         build_indices/0,
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
-include("apps/ghostlight/include/ghostlight_data.hrl").


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


-record(index_config, {
          listings_call,
          resource_module,
          type_name,
          id_fun
         }).


build_indices() ->
    ok.


populate_indices() ->
    ResourceConfigs = [
                       #index_config{
                          listings_call = get_show_listings,
                          resource_module = ghostlight_show,
                          type_name = <<"shows">>,
                          id_fun = fun(#show{id=ShowId}) -> ShowId end
                       },
                       #index_config{
                          listings_call = get_org_listings,
                          resource_module = ghostlight_org,
                          type_name = <<"organizations">>,
                          id_fun = fun(#organization{id=OrgId}) -> OrgId end
                       },
                       #index_config{
                          listings_call = get_work_listings,
                          resource_module = ghostlight_work,
                          type_name = <<"pieces">>,
                          id_fun = fun(#work{id=WorkId}) -> WorkId end
                       },
                       #index_config{
                          listings_call = get_person_listings,
                          resource_module = ghostlight_people,
                          type_name = <<"people">>,
                          id_fun = fun(#person{id=PersonId}) -> PersonId end
                       }
                      ],
    lists:foreach(fun populate_index/1, ResourceConfigs).

populate_index(#index_config{
          listings_call=ListingsCall,
          resource_module=ResourceModule,
          type_name=TypeName,
          id_fun=IdFun
          }) ->
    ResourceList = apply(ghostlight_db, ListingsCall, []),
    AlmostReady = [ {IdFun(Resource), ResourceModule:record_to_json(Resource)} || Resource <- ResourceList],
    AsStrings = [ {ResourceId, jiffy:encode(ResourceProplist)} || {ResourceId, ResourceProplist} <- AlmostReady],
    lists:foreach(fun({ResourceId, ResourceJSON}) ->
                          erlastic_search:index_doc_with_id(<<"ghostlight">>, TypeName, ResourceId, ResourceJSON)
                  end, AsStrings).



