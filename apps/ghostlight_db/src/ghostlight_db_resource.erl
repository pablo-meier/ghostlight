%%%-------------------------------------------------------------------
%%% @author pablo
%%% @doc
%%% Abstracts out the bits of the gen_servers for the resources.
%%% @end
%%%-------------------------------------------------------------------
-module(ghostlight_db_resource).

-behaviour(gen_server).

%% API
-export([start_link/0,
        get/2,
        get/3,
        listings/1,
        insert/2,
        update/2
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-include("apps/ghostlight/include/ghostlight_data.hrl").
-include("apps/ghostlight_db/include/ghostlight_db_statements.hrl").

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    C = ghostlight_db_utils:connect_to_postgres(),
    DbState = ghostlight_db_utils:get_state(C),
    {ok, DbState}.

handle_call({get, Resource, ResourceId}, _From, DbState) ->
    GetStatement = Resource:get_statement(DbState),
    Batch = [ {GetStatement, [ResourceId]} ],
    Reply = ghostlight_db_utils:exec_batch(Batch, DbState),
    {reply, Reply, DbState};

handle_call({listings, Resource}, _From, DbState=#db_state{connection=C}) ->
    ListingsStatement = Resource:listings_statement(DbState),
    epgsql:bind(C, ListingsStatement, "", []),
    {ok, Rows} = epgsql:execute(C, ListingsStatement),
    {reply, Rows, DbState};

handle_call({insert, Resource, Record}, _From, DbState) ->
    {Inserts, Id} = Resource:get_inserts(Record, DbState),
    Reply = ghostlight_db_utils:exec_batch(Inserts, DbState),
    lager:info("Postgres returned ~p for insert", [Reply]),
    {reply, Id, DbState};

handle_call({update, Resource, Record}, _From, DbState) ->
    Commands = Resource:get_update_commands(Record, DbState),
    Reply = ghostlight_db_utils:exec_batch(Commands, DbState),
    lager:info("Postgres returned ~p for update", [Reply]),
    {reply, true, DbState};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #db_state{connection=C}) ->
    lager:error("DB server (RESOURCE) terminating: ~p", [Reason]),
    epgsql:close(C).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% API
%%%===================================================================

%%% @doc
%%% Gets a single resource from the DB as HTML
%%% @end
get(Resource, ResourceId) ->
    get(Resource, ResourceId, html).

%%% @doc
%%% Gets a single resource from the DB as either HTML or Markdown source.
%%% @end
get(Resource, ResourceId, Format) ->
    case ghostlight_db_utils:is_valid_uuid(ResourceId) of
        true -> 
            Response = gen_server:call(?MODULE, {get, Resource, ResourceId}),
            process_db_response(Resource, Response, Format);
        false -> throw(not_found)
    end.

%%% @doc
%%% Gets all of a resource from the DB as HTML
%%% @end
listings(Resource) ->
    Response = gen_server:call(?MODULE, {listings, Resource}),
    Resource:db_listings_to_record_list(Response).


%%% @doc
%%% Inserts the record into the appropriate table.
%%% @end
insert(Resource, Record) ->
    gen_server:call(?MODULE, {insert, Resource, Record}).


%%% @doc
%%% Updates the record into the appropriate table.
%%% @end
update(Resource, Record) ->
    gen_server:call(?MODULE, {update, Resource, Record}).


%%%===================================================================
%%% Internal functions
%%%===================================================================

process_db_response(
  _Module,
  [{ok, []},
   {ok, []},
   {ok, []}], _Form) ->
    throw(not_found);

process_db_response(Module, Response, Format) ->
    Module:db_to_record(Response, Format).


%% Alright, here are things that need to happen:
%% * CALLBACK to get the appropriate "get_/get_listings/insert/update"
%%   statements from the record, run it on the appropriate ID.
%% * Check UUID format, 404s as necessary.
%% * CALLBACK to package DB response as record.
%%
%% * Versioning?

