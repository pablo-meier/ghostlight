%% Here's the expectation from the DB on getting a show:
%%
%% * What is it? Work, org, dates.  TODO: Venue.
%% * Who was in it and who worked on it? Actors, directors, stage hands, etc.
%%
%% Later maybe we could do things like who went/who's going, or sorting by
%% shows that haven't closed yet.
-module(ghostlight_db_show).
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

         get_inserts/1,
         prepare_statements/2]).

-define(SERVER, ?MODULE).

-include("apps/ghostlight/include/ghostlight_data.hrl").

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
init([]) ->
    C = ghostlight_db_utils:connect_to_postgres(),
    State = ghostlight_db_utils:get_state(C),
    {ok, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(Reason, #db_state{connection=C}) ->
    lager:error("DB server (SHOWS) terminating: ~p", [Reason]),
    epgsql:close(C).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({insert_show, Show}, _From, State) ->
    {Inserts, Id} = get_inserts(Show, State),
    Reply = ghostlight_db_utils:exec_batch(Inserts, State),
    lager:info("Postgres returned ~p for show insert", [Reply]),
    {reply, Id, State};


handle_call(get_show_listings, _From, State=#db_state{connection=C, get_show_listings=GL}) ->
    epgsql:bind(C, GL, "", []),
    {ok, Rows} = epgsql:execute(C, GL),
    {reply, Rows, State};


handle_call({get_show, ShowId}, _From, State=#db_state{get_show_meta=SM,
                                                    get_show_onstage=SO,
                                                    get_show_offstage=SOff,
                                                    get_show_authorship=SA,
                                                    get_show_directors=SD}) ->
    Batch = [ {SM, [ShowId]},
              {SO, [ShowId]},
              {SOff, [ShowId]},
              {SA, [ShowId]},
              {SD, [ShowId]}],
    Reply = ghostlight_db_utils:exec_batch(Batch, State),
    {reply, Reply, State};

handle_call({get_inserts, Show}, _From, State) ->
    get_inserts(Show, State);

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

get_inserts(#show{title=Title,
                  org=Org,
                  performances=Performances,
                  special_thanks=SpecialThanks,
                  dates=Dates
            },
            State=#db_state{insert_show_statement=IS,
                            insert_dates_statement=ID}) ->
    Works = extract_works(Performances),
    {AllWorkInserts, WorksWithId} = fold_over_works(Works),
    {OrgInserts, OrgId} = ghostlight_db_org:get_inserts(Org),
    ShowId = ghostlight_db_utils:fresh_uuid(),
    ShowInserts = [{IS, [ShowId, Title, OrgId, SpecialThanks]}],
    DateInserts = lists:map(fun (Date) ->
                                {ID, [ShowId, Date]}
                            end, Dates),
    AllPerformanceInserts = fold_over_performances(Performances, ShowId, WorksWithId, State),
    Batch = lists:append([
                          AllWorkInserts,
                          OrgInserts,
                          ShowInserts,
                          DateInserts,
                          AllPerformanceInserts
                         ]),
    {Batch, ShowId}.

extract_works(Performances) ->
    lists:map(fun (#performance{work = Work}) -> Work end, Performances).

get_performance_inserts(WorksWithIds,
                        ShowId,
                        PerformanceOrder,
                        #performance{ 
                           work=Work,
                           onstage=Onstage,
                           offstage=Offstage,
                           directors=Directors },
                        State=#db_state{insert_performance_statement=IP}) ->
    PerformanceId = ghostlight_db_utils:fresh_uuid(),
    WorkId = proplists:get_value(Work, WorksWithIds),
    PerformanceInserts = [{IP, [PerformanceId, WorkId, ShowId, PerformanceOrder]}],
    DirectorInserts = get_director_inserts(PerformanceId, Directors, State),
    OnstageInserts = get_onstage_inserts(PerformanceId, Onstage, State),
    OffstageInserts = get_offstage_inserts(PerformanceId, Offstage, State),

    Inserts = lists:append([PerformanceInserts,
                            DirectorInserts,
                            OnstageInserts,
                            OffstageInserts]),
    {Inserts, PerformanceId}.


get_director_inserts(PerformanceId, Directors, #db_state{insert_director_statement=ID}) ->
    ListOfLists = lists:map(fun (Person) ->
                                {PersonInserts, PersonId} = ghostlight_db_person:get_inserts(Person),
                                DirectorInsert = {ID, [PerformanceId, PersonId]},
                                [PersonInserts, DirectorInsert]
                            end, Directors),
    lists:flatten(ListOfLists).

get_onstage_inserts(PerformanceId, OnstageList, #db_state{insert_onstage_statement=IO}) ->
    ListOfLists = lists:map(fun (#onstage{ role=Role,
                                           person=Person }) ->
                                {PersonInserts, PersonId} = ghostlight_db_person:get_inserts(Person),
                                OnstageInsert = {IO, [PerformanceId, PersonId, Role, null, null, null]},
                                [PersonInserts, OnstageInsert]
                            end, OnstageList),
    lists:flatten(ListOfLists).


get_offstage_inserts(PerformanceId, OffstageList, #db_state{insert_offstage_statement=IO}) ->
    ListOfLists = lists:map(fun (#offstage{ job=Job,
                                            person=Person }) ->
                                {PersonInserts, PersonId} = ghostlight_db_person:get_inserts(Person),
                                OffstageInsert = {IO, [PerformanceId, PersonId, Job, null, null]},
                                [PersonInserts, OffstageInsert]
                            end, OffstageList),
    lists:flatten(ListOfLists).


fold_over_works(Works) ->
    lists:foldl(fun (Work, {Inserts, Accum}) ->
                        {WorkInserts, WorkId} = ghostlight_db_work:get_inserts(Work),
                        {lists:append(WorkInserts, Inserts),
                         [{Work, WorkId}|Accum]}
                end, {[], []}, Works).

fold_over_performances(Performances, ShowId, WorksWithIds, State) ->
    Numbered = lists:zip(Performances, lists:seq(1, length(Performances))),
    {FinalInserts, _} = lists:foldl(fun ({Perf, Order}, {Inserts, Accum}) ->
                           {PerfInserts, PerfId} = get_performance_inserts(WorksWithIds, ShowId, Order, Perf, State),
                           {lists:append(Inserts, PerfInserts), [{Perf, PerfId}|Accum]}
                   end, {[], []}, Numbered),
    FinalInserts.


%%%===================================================================
%%% Resource callbacks.
%%%===================================================================
get(ShowId) ->
    Response = gen_server:call(?MODULE, {get_show, ShowId}),
    %% handle if this call fails.
    [_, {ok, Meta}, {ok, Onstage}, {ok, Offstage}, {ok, Authorship}, {ok, Directors}, _] = Response,
    [{Title, OrgName, OrgId, SpecialThanks, _}|_] = Meta,
    Dates = [ Date || {_, _, _, _, Date} <- Meta ],

    Authors = authors_to_map(Authorship),
    DirectorMap = authors_to_map(Directors),
    #show{
        title = Title,
        special_thanks = SpecialThanks,
        dates = Dates,
        org = #organization{
                   id = OrgId,
                   name = OrgName
                },
        performances = make_performance_record_list(Onstage, Offstage, Authors, DirectorMap)
      }.

make_performance_record_list(Onstage, Offstage, AuthorMap, DirectorMap) ->
    PerformancesMap = lists:foldl(fun ({WorkId, Title, PerformerName, PerformerId, Role}, Accum) ->
                                      PerformerRecord = #onstage{
                                                           role = Role,
                                                           person = #person{
                                                                       id = PerformerId,
                                                                       name = PerformerName
                                                                    }
                                                        },
                                      case maps:get({WorkId, Title}, Accum, none) of
                                          none ->
                                              maps:put({WorkId, Title}, [PerformerRecord], Accum);
                                          Performers ->
                                              maps:put({WorkId, Title}, [PerformerRecord|Performers], Accum)
                                      end
                                  end, maps:new(), Onstage),

    OffstageMap = lists:foldl(fun ({WorkId, Title, Job, PersonId, PersonName}, Accum) ->
                                  PersonRecord = #offstage{
                                                    job = Job,
                                                    person = #person{
                                                                id = PersonId,
                                                                name = PersonName 
                                                             }
                                                 },
                                  case maps:get({WorkId, Title}, Accum, none) of
                                      none ->
                                          maps:put({WorkId, Title}, [PersonRecord], Accum);
                                      People ->
                                          maps:put({WorkId, Title}, [PersonRecord|People], Accum)
                                  end
                              end, maps:new(), Offstage),

    [ #performance{
          work = #work {
                     id = WorkId,
                     title = Title,
                     authors = maps:get(Title, AuthorMap)
                 },
          onstage = maps:get({WorkId, Title}, PerformancesMap, []),
          offstage = maps:get({WorkId, Title}, OffstageMap, []),
          directors = maps:get(Title, DirectorMap, [])
      } || {WorkId, Title} <- maps:keys(PerformancesMap) ].


%% Given a list of Authorship like the one we return from SQL, gives us a map where every key is the title
%% of a work, and the value is a list of all its authors.
authors_to_map(AuthorList) ->
    lists:foldl(fun ({Title, AuthorId, Author}, Accum) ->
            AuthorTuple = #person{id=AuthorId, name=Author},
            case maps:get(Title, Accum, none) of
                none ->
                    maps:put(Title, [AuthorTuple], Accum);
                Authors ->
                    maps:put(Title, [AuthorTuple|Authors], Accum)
            end
        end, maps:new(), AuthorList).


listings() ->
    Response = gen_server:call(?MODULE, get_show_listings),
    [ #show{
         id=ShowId,
         title=ShowName,
         org=#organization{
                id=OrgId,
                name=OrgName
               }
        } || {ShowId, ShowName, OrgId, OrgName} <- Response ].

insert(Show) ->
    gen_server:call(?MODULE, {insert_show, Show}).

get_inserts(Show) ->
    gen_server:call(?MODULE, {get_inserts, Show}).


prepare_statements(C, State) ->
    PerformanceSql = "INSERT INTO performances (performance_id, work_id, show_id, performance_order)"
        ++ " VALUES($1, $2, $3, $4)",
    {ok, InsertPerformance} = epgsql:parse(C, "insert_performance", PerformanceSql, [uuid, uuid, uuid, int4]),

    DirectorSql = "INSERT INTO performance_directors (performance_id, director_id) VALUES($1, $2)",
    {ok, InsertDirector} = epgsql:parse(C, "insert_performance_director", DirectorSql, [uuid, uuid]),

    OnstageSql = "INSERT INTO performance_onstage (performance_id, performer_id, role, understudy_id, date_started, date_ended)"
        ++ " VALUES($1, $2, $3, $4, $5, $6)",
    {ok, InsertOnstage} = epgsql:parse(C, "insert_performance_onstage", OnstageSql, [uuid, uuid, text, uuid, date, date]),

    OffstageSql = "INSERT INTO performance_offstage (performance_id, person_id, job, date_started, date_ended)"
        ++ " VALUES($1, $2, $3, $4, $5)",
    {ok, InsertOffstage} = epgsql:parse(C, "insert_performance_offstage", OffstageSql, [uuid, uuid, text, date, date]),

    ShowSql = "INSERT INTO shows (show_id, title, producing_org_id, special_thanks, date_created) "
        ++ " VALUES($1, $2, $3, $4, CURRENT_DATE)",
    {ok, InsertShow} = epgsql:parse(C, "insert_show", ShowSql, [uuid, text, uuid, text]),

    DatesSql = "INSERT INTO show_dates (show_id, show_date) VALUES($1, $2)",
    {ok, InsertDates} = epgsql:parse(C, "insert_show_dates", DatesSql, [uuid, timestamptz]),

    %% SELECT Statements
    %% This will get the show's metadata
    GetShowSql = "SELECT s.title, o.name, o.org_id, s.special_thanks, d.show_date "
        ++ "FROM shows AS s INNER JOIN organizations AS o ON (s.producing_org_id = o.org_id) "
        ++ "INNER JOIN show_dates AS d USING (show_id) WHERE s.show_id = $1",
    {ok, GetShow} = epgsql:parse(C, "get_show_meta", GetShowSql, [uuid]),

    %% This monstrosity will get all the performers in a show, even if spanning
    %% Many performances.
    GetOnstageSql = "SELECT w.work_id, w.title, "
        ++ "people2.name AS performer_name, people2.person_id AS performer_id, po.role "
        ++ "FROM works AS w INNER JOIN performances AS p USING (work_id) "
        ++ "INNER JOIN performance_onstage AS po USING (performance_id) "
        ++ "INNER JOIN people AS people2 ON (people2.person_id = po.performer_id) "
        ++ "WHERE p.show_id = $1 "
        ++ "GROUP BY w.work_id, people2.person_id, p.performance_id, w.title, "
        ++ "people2.name, po.role ORDER BY p.performance_order;",
    {ok, GetOnstage} = epgsql:parse(C, "get_show_onstage", GetOnstageSql, [uuid]),

    GetOffstageSql = "SELECT w.work_id, w.title, off.job, person.person_id, person.name FROM performance_offstage AS off "
        ++ "INNER JOIN people AS person USING (person_id) INNER JOIN performances AS p USING (performance_id) "
        ++ "INNER JOIN shows USING (show_id) INNER JOIN works AS w ON (w.work_id = p.work_id) WHERE show_id = $1",
    {ok, GetOffstage} = epgsql:parse(C, "get_show_offstage", GetOffstageSql, [uuid]),

    %% Pulls the authors of a show.
    GetAuthorsSql = "SELECT w.title, p.person_id AS author_id, p.name AS author_name FROM "
        ++ "(SELECT p.work_id FROM performances AS p WHERE p.show_id = $1) AS works_in_show "
        ++ "INNER JOIN works AS w USING (work_id) INNER JOIN authorship AS a USING (work_id) "
        ++ "INNER JOIN people AS p USING (person_id)",
    {ok, GetAuthors} = epgsql:parse(C, "get_show_authors", GetAuthorsSql, [uuid]),

    %% Pulls the directors of a show.
    GetDirectorsSql = "SELECT w.title, p.person_id AS director_id, p.name AS director_name FROM "
        ++ "works AS w INNER JOIN performances AS perf USING (work_id) INNER JOIN "
        ++ "performance_directors AS pd USING (performance_id) INNER JOIN people AS p "
        ++ "ON (p.person_id = pd.director_id) INNER JOIN shows AS s ON (perf.show_id = s.show_id) "
        ++ "WHERE s.show_id = $1",
    {ok, GetDirectors} = epgsql:parse(C, "get_show_directors", GetDirectorsSql, [uuid]),
 
    %% For show listings -- much like the meta of a single one.
    GetShowListingsSql = "SELECT s.show_id, s.title, o.org_id, o.name "
        ++ "FROM shows AS s INNER JOIN organizations AS o ON (s.producing_org_id = o.org_id) "
        ++ "LIMIT 30",
    {ok, GetShowListings} = epgsql:parse(C, "show_listings_meta", GetShowListingsSql, []),


 
    State#db_state{
       insert_performance_statement=InsertPerformance,
       insert_director_statement=InsertDirector,
       insert_onstage_statement=InsertOnstage,
       insert_offstage_statement=InsertOffstage,
       insert_show_statement=InsertShow,
       insert_dates_statement=InsertDates,
       
       get_show_listings=GetShowListings,
       get_show_meta=GetShow,
       get_show_onstage=GetOnstage,
       get_show_offstage=GetOffstage,
       get_show_authorship=GetAuthors,
       get_show_directors=GetDirectors
    }.

