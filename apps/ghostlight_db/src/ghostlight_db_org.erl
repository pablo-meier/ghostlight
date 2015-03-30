%% Orgs are pretty straightforward -- who's in them, and what have they
%% produced. Need to see if I care about in-app membership enough to 
%% make that happen here.
-module(ghostlight_db_org).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([get/1,
         listings/0,
         insert/1,
         get_inserts/1]).

-define(SERVER, ?MODULE).

-include("apps/ghostlight/include/ghostlight_data.hrl").

-record(state, {connection,
                begin_statement,
                commit_statement,

                get_org_listings,
                insert_org_statement,
                get_org_meta,
                get_org_show_dates,
                get_produced_by_org,
                get_org_employees


               }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
init([]) ->
    C = ghostlight_db_utils:connect_to_postgres(),
    State = prepare_statements(C),
    {ok, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(Reason, #state{connection=C}) ->
    lager:error("DB server (ORGS) terminating: ~p", [Reason]),
    epgsql:close(C).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(get_org_listings, _From, State=#state{connection=C, get_org_listings=GO}) ->
    epgsql:bind(C, GO, "", []),
    {ok, Rows} = epgsql:execute(C, GO),
    {reply, Rows, State};

handle_call({get_org, OrgId}, _From, State=#state{get_org_meta=GOM,
                                                  get_org_show_dates=GOD,
                                                   get_produced_by_org=GPO,
                                                   get_org_employees=GOE}) ->
    Batch = [ {GOM, [OrgId]},
              {GOD, [OrgId]},
              {GPO, [OrgId]},
              {GOE, [OrgId]} ],
    Reply = exec_batch(Batch, State),
    {reply, Reply, State};

handle_call({insert_org, Org}, _From, State) ->
    {Inserts, _Ids} = get_inserts(Org),
    Reply = exec_batch(Inserts, State),
    {reply, Reply, State};


handle_call({get_inserts,
             #organization {
                    name=Name,
                    tagline=Tagline,
                    description=Description,
                    parent=Parent,
                    vanity_name=VanityName,
                    date_founded=DateFounded,
                    visibility=Visibility
               }},
            _From, 
            State=#state{insert_org_statement=IO}) ->

    OrgId = ghostlight_db_utils:fresh_uuid(),
    ParentInsert = case Parent of
                       {id, <<"">>} -> null;
                       {id, Valid} -> Valid
                   end,
    DescriptionInsert = ghostlight_db_utils:null_if_unspecified(Description),
    VanityNameInsert = ghostlight_db_utils:null_if_unspecified(VanityName),
    TaglineInsert = ghostlight_db_utils:null_if_unspecified(Tagline),
    DateFoundedInsert = ghostlight_db_utils:null_if_unspecified(DateFounded),

    OrgInserts = [{IO, [OrgId, ParentInsert, Name, TaglineInsert, DescriptionInsert, VanityNameInsert, DateFoundedInsert, Visibility]}],
    Reply = {OrgInserts, OrgId},
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.



%%%===================================================================
%%% Resource callbacks.
%%%===================================================================
get(OrgId) ->
    Reply = gen_server:call(?MODULE, {get_org, OrgId}),
    [{ok, []}, {ok, Meta}, {ok, Dates}, {ok, Produced}, {ok, Employees}, {ok, []}] = Reply,
    [{Name, Tagline, Description}] = Meta,
    DateMap = lists:foldl(fun({Id, Title, Date}, Accum) ->
                                  case maps:get({Id, Title}, Accum, undefined) of
                                      undefined -> maps:put({Id, Title}, [Date], Accum);
                                      DateList -> maps:put({Id, Title}, [Date|DateList], Accum)
                                  end
                          end, maps:new(), Dates),
    ShowList = condense_performances_in_show(Produced, DateMap),
    EmployeeList = [ #org_employee{
                        title=PersonTitle,
                        person=#person{
                            id=PersonId,
                            name=PersonName
                        }
                     } || {PersonId, PersonName, PersonTitle} <- Employees ],
    #org_return{
      org=#organization{
             id=OrgId,
             name=Name,
             tagline=Tagline,
             description=Description
          },
      shows_produced=ShowList,
      employees=EmployeeList
    }.

condense_performances_in_show(ShowList, DateMap) ->
    AsMap = lists:foldl(fun ({ShowId, ShowTitle, WorkId, WorkTitle}, Accum) ->
                                NewPerformance = #performance{work=#work{id=WorkId, title=WorkTitle}},
                                case maps:get({ShowId, ShowTitle}, Accum, none) of
                                    none ->
                                        maps:put({ShowId, ShowTitle}, [NewPerformance], Accum);
                                    PerformanceList ->
                                        maps:put({ShowId, ShowTitle}, [NewPerformance|PerformanceList], Accum)
                                end
                        end, maps:new(), ShowList),
    WithDates = [ #show{
                    id = ShowId,
                    title = ShowTitle,
                    performances = lists:reverse(maps:get({ShowId, ShowTitle}, AsMap)),
                    dates = maps:get({ShowId, ShowTitle}, DateMap)
                 } || {ShowId, ShowTitle} <- maps:keys(AsMap) ],
    lists:sort(fun(#show{dates=[D1|_]}, #show{dates=[D2|_]}) ->
                       D1 >= D2
               end, WithDates).

listings() ->
    Results = gen_server:call(?MODULE, get_org_listings),
    [ #organization{
         id=OrgId,
         name=OrgName,
         tagline=Tagline,
         description=Description
        } || {OrgId, OrgName, Tagline, Description} <- Results].

insert(Org) ->
    gen_server:call(?MODULE, {insert_org, Org}).
get_inserts(Org) ->
    gen_server:call(?MODULE, {get_inserts, Org}).


prepare_statements(C) ->
    {ok, BeginStmt} = epgsql:parse(C, "begin_statement", "BEGIN", []),
    {ok, CommitStmt} = epgsql:parse(C, "commit_statement", "COMMIT", []),

    OrgsSql = "INSERT INTO organizations (org_id, parent_org, name, tagline, description, vanity_name, date_founded, visibility)"
        ++ " VALUES($1, $2, $3, $4, $5, $6, $7::date, $8)",
    {ok, InsertOrg} = epgsql:parse(C, "insert_organization", OrgsSql, [uuid, uuid, text, text, text, text, date, text]),


    GetOrgMetaSql = "SELECT o.name, o.tagline, o.description FROM organizations AS o WHERE o.org_id = $1",
    {ok, GetOrgMeta} = epgsql:parse(C, "get_org_meta", GetOrgMetaSql, [uuid]),

    GetProducedByOrgSql = "SELECT s.show_id, s.title, w.work_id, w.title FROM shows AS s "
        ++ "INNER JOIN performances AS p USING (show_id) INNER JOIN organizations AS o ON (o.org_id = s.producing_org_id) "
        ++ "INNER JOIN works AS w ON (p.work_id = w.work_id) WHERE o.org_id = $1",
    {ok, GetProducedByOrg} = epgsql:parse(C, "get_produced_by_org", GetProducedByOrgSql, [uuid]),

    GetDatesOfShowSql = "SELECT s.show_id, s.title, d.show_date FROM shows AS s INNER JOIN "
        ++ "organizations AS o ON (o.org_id = s.producing_org_id) INNER JOIN show_dates AS d "
        ++ "USING (show_id) WHERE o.org_id = $1 ORDER BY d.show_date ASC",
    {ok, GetDatesOfShow} = epgsql:parse(C, "get_dates_of_shows_by_org", GetDatesOfShowSql, [uuid]),

    GetOrgEmployeesSql = "SELECT p.person_id, p.name, oe.title FROM org_employees AS oe INNER JOIN people AS p USING "
        ++ "(person_id) WHERE oe.org_id = $1",
    {ok, GetOrgEmployees} = epgsql:parse(C, "get_org_employees", GetOrgEmployeesSql, [uuid]),

    GetOrgListingsSql = "SELECT o.org_id, o.name, o.tagline, o.description FROM organizations AS o ORDER BY o.name ASC LIMIT 50",
    {ok, GetOrgListings} = epgsql:parse(C, "get_org_listings", GetOrgListingsSql, []),

 
    #state{
       connection=C,
       begin_statement=BeginStmt,
       commit_statement=CommitStmt,

       insert_org_statement=InsertOrg,
       get_org_listings=GetOrgListings,
       get_org_meta=GetOrgMeta,
       get_produced_by_org=GetProducedByOrg,
       get_org_show_dates=GetDatesOfShow,
       get_org_employees=GetOrgEmployees
    }.


%% TODO Put this in one place? The #state bit makes it hard tho.
exec_batch(Batch, #state{connection=C,
                         commit_statement=COMMIT,
                         begin_statement=BEGIN}) ->
    AsTransaction = lists:append([ [{BEGIN, []}],
                                   Batch,
                                   [{COMMIT, []}] ]),
    Results = epgsql:execute_batch(C, AsTransaction),
    Results.

