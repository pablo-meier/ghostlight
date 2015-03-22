%%% The DATABASE
%%%
%%% The primary value-prop of Ghostlight comes from its database. Virtually
%%% _everything_ that gives it value over a pile of programs comes from the
%%% fact that a) we have a well-maintained, well-run database containing all
%%% the information, and b) that the data is good.
%%%
%%% While b) is mostly a policy issue (and some data science I don't know),
%%% most of a) gets handled here!
%%%
%%% Here is the high-level on the database:
%%% * It exposes a number of API functions to the app that are mostly wrappers
%%%   to gen_server:call/cast. Technically speaking, we can change everything
%%%   about this module EXCEPT this -- we could change DB engines, ditch
%%%   gen_server, whatever. Highly unlikely, but that's the hard module
%%%   boundary.
%%% * We make it a gen_server for the well-known obvious reasons: we can restart
%%%   it, supervise it, etc., and it's a solid way to maintain the state that
%%%   we'll need (parsed queries, PSQL connections).
%%%
%%% Since the data is so spread out with a myriad of hairy relationships, 
%%% generating the exact queries for the use case requires something of a 
%%% calculus.
%%%
%%% Technically speaking, that's what's 
%%%
%%%
%%% Some ground rules for the module:
%%%
%%% * SQL is invisible. No evidence of the statements/queries or the rows and
%%%   row structure should escape here. We trade entirely in the records
%%%   specified in ghostlight_data.
%%%
-module(ghostlight_db).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([insert_show/1,
         get_show/1,
         get_person/1]).  

-include("apps/ghostlight/include/ghostlight_data.hrl").

-define(SERVER, ?MODULE).
-record(state, {connection,
                begin_statement,
                commit_statement,

                insert_work_statement,
                insert_authorship_statement,
                insert_person_statement,
                insert_org_statement,
                insert_performance_statement,
                insert_onstage_statement,
                insert_offstage_statement,
                insert_show_statement,
                insert_dates_statement,

                get_show_meta,
                get_show_onstage,
                get_show_authorship
                }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, C} = epgsql:connect("localhost", "pablo", "", [{database, "ghostlight-dev"}]),
    initialize_tables(C),
    State = prepare_statements(C),
    {ok, State}.

handle_call({insert_show, Show}, _From, State) ->
    Inserts = get_show_inserts(Show, State),
    Reply = exec_batch(Inserts, State),
    {reply, Reply, State};

handle_call({get_show, ShowId}, _From, State=#state{get_show_meta=SM, get_show_onstage=SO, get_show_authorship=SA}) ->
    Batch = [ {SM, [ShowId]},
              {SO, [ShowId]},
              {SA, [ShowId]}],
    Reply = exec_batch(Batch, State),
    {reply, Reply, State};

handle_call({get_person, PersonId}, _From, State=#state{}) ->
    %% A person could:
    %% * have been an onstage performer
    %% * been an offstage contributor
    %% * be an author of a work
    %% * be a director of a performance
    %% * be affiliated with an org
    %%
    %% Query should fetch all of this and make it presentable.
    Reply = <<"ok">>,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #state{connection=C}) ->
    lager:error("DB server terminating: ~p", [Reason]),
    epgsql:close(C).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Smooth-n-easy API for the normies
%%%===================================================================

insert_show(Show) ->
    gen_server:call(?MODULE, {insert_show, Show}).

get_show(ShowId) ->
    Response = gen_server:call(?MODULE, {get_show, ShowId}),
    %% handle if this call fails.
    [_, {ok, Meta}, {ok, Performances}, {ok, Authorship}, _] = Response,
    [{Title, OrgName, SpecialThanks, Date}] = Meta,

    Authors = authors_to_map(Authorship),

    Return = #show{
                 title = Title,
                 special_thanks = SpecialThanks,
                 dates = [Date],
                 org = #organization{
                            name = OrgName
                         },
                 performances = make_performance_record_list(Performances, Authors)
               },
    Return.

make_performance_record_list(Performances, AuthorMap) ->
    PerformancesMap = lists:foldl(fun ({Title, _, Performer, Role}, Accum) ->
                                      PerformerRecord = #onstage{
                                                           role = Role,
                                                           person = {name, Performer}
                                                        },
                                      case maps:get(Title, Accum, none) of
                                          none ->
                                              maps:put(Title, [PerformerRecord], Accum);
                                          Performers ->
                                              maps:put(Title, [PerformerRecord|Performers], Accum)
                                      end
                                  end, maps:new(), Performances),

    DirectorMap = lists:foldl(fun ({Title, Director, _, _}, Accum) ->
                                  case maps:get(Title, Accum, none) of
                                      none ->
                                          maps:put(Title, {name, Director}, Accum);
                                      {name, _} ->
                                          Accum
                                  end
                              end, maps:new(), Performances),
    lists:map(fun (Title) ->
                  #performance{
                      work = #work {
                                 title = Title,
                                 authors = maps:get(Title, AuthorMap)
                             },
                      onstage = maps:get(Title, PerformancesMap),
                      directors = [maps:get(Title, DirectorMap)]
                  }
              end, maps:keys(PerformancesMap)).


%% Given a list of Authorship like the one we return from SQL, gives us a map where every key is the title
%% of a work, and the value is a list of all its authors.
authors_to_map(AuthorList) ->
    lists:foldl(fun ({Title, Author}, Accum) ->
            AuthorTuple = {name, Author},
            case maps:get(Title, Accum, none) of
                none ->
                    maps:put(Title, [AuthorTuple], Accum);
                Authors ->
                    maps:put(Title, [AuthorTuple|Authors], Accum)
            end
        end, maps:new(), AuthorList).


get_person(PersonId) ->
    _Response = gen_server:call(?MODULE, {get_person, PersonId}),
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

initialize_tables(_Connection) ->
    ok.

exec_batch(Batch, #state{connection=C,
                        commit_statement=COMMIT,
                        begin_statement=BEGIN}) ->
    AsTransaction = lists:append([ [{BEGIN, []}],
                                   Batch,
                                   [{COMMIT, []}] ]),
    Results = epgsql:execute_batch(C, AsTransaction),
    Results.

%% Given how the keys reference each other, it's kind of critical that 
%% the operations are done in this order.
get_show_inserts(#show{title=Title,
                       org=Org,
                       performances=Performances,
                       special_thanks=SpecialThanks,
                       dates=Dates
                      },
                 State=#state{ insert_show_statement=IS,
                               insert_dates_statement=ID}) ->
    Works = extract_works(Performances),
    {AllWorkInserts, WorksWithId} = fold_over_works(Works, State),
    {OrgInserts, OrgId} = get_producing_org_inserts(Org, State),
    ShowId = fresh_uuid(),
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
    Batch.


extract_works(Performances) ->
    lists:map(fun (#performance{work = Work}) -> Work end, Performances).

%% This looks worse than it is -- it's a generalized fold over the insert generators. So
%% wheraas `get_X_inserts` return {XInserts, Id}, at a high level we'd like a way to get
%% all the inserts [X] and their IDs. The return Tuple is {AllInserts, [{Item, Id}]
fold_over_insertable(Fun, Collection, State) ->
    lists:foldl(fun (Item, {Inserts, Accum}) ->
                         {NewInserts, NewId} = Fun(Item, State),
                         {lists:append(Inserts, NewInserts), [{Item, NewId}|Accum]}
                 end, {[], []}, Collection).

fold_over_works(Works, State) ->
    fold_over_insertable(fun get_work_inserts/2, Works, State).

fold_over_performances(Performances, ShowId, WorksWithIds, State) ->
    Partialed = fun ({P, Order}, S) ->
                        get_performance_inserts(WorksWithIds, ShowId, Order, P, S)
                end,
    Numbered = lists:zip(Performances, lists:seq(1, length(Performances))),
    {Inserts, _} = fold_over_insertable(Partialed, Numbered, State),
    Inserts.


get_performance_inserts(WorksWithIds,
                        ShowId,
                        PerformanceOrder,
                        #performance{ 
                           work=Work,
                           onstage=Onstage,
                           offstage=Offstage,
                           directors=Directors },
                        State=#state{insert_performance_statement=IP}) ->
    PerformanceId = fresh_uuid(),
    WorkId = proplists:get_value(Work, WorksWithIds),
    {DirectorInserts, [DirectorId]} = get_people_inserts(Directors, State),
    PerformanceInserts = [{IP, [PerformanceId, WorkId, ShowId, DirectorId, PerformanceOrder]}],
    OnstageInserts = get_onstage_inserts(PerformanceId, Onstage, State),
    OffstageInserts = get_offstage_inserts(PerformanceId, Offstage, State),

    Inserts = lists:append([DirectorInserts,
                            PerformanceInserts,
                            OnstageInserts,
                            OffstageInserts]),
    {Inserts, PerformanceId}.


get_onstage_inserts(PerformanceId, OnstageList, State=#state{insert_onstage_statement=IO}) ->
    ListOfLists = lists:map(fun (#onstage{ role=Role,
                                           person=Person }) ->
                                {PersonInserts, [PersonId]} = get_people_inserts([Person], State),
                                OnstageInsert = {IO, [PerformanceId, PersonId, Role, null, null, null]},
                                lists:reverse([OnstageInsert|PersonInserts])
                            end, OnstageList),
    lists:flatten(ListOfLists).


get_offstage_inserts(PerformanceId, OffstageList, State=#state{insert_offstage_statement=IO}) ->
    ListOfLists = lists:map(fun (#offstage{ job=Job,
                                            person=Person }) ->
                                {PersonInserts, [PersonId]} = get_people_inserts([Person], State),
                                OffstageInsert = {IO, [PerformanceId, PersonId, Job, null, null, null]},
                                lists:reverse([OffstageInsert|PersonInserts])
                            end, OffstageList),
    lists:flatten(ListOfLists).


get_producing_org_inserts(#organization {
                             name=Name,
                             tagline=Tagline,
                             description=Description,
                             parent=Parent,
                             vanity_name=VanityName,
                             date_founded=DateFounded,
                             visibility=Visibility
                          },
                          #state{insert_org_statement=IO}) ->
    OrgId = fresh_uuid(),
    ParentInsert = case Parent of
                       {id, <<"">>} -> null;
                       {id, Valid} -> Valid
                   end,
    DescriptionInsert = null_if_unspecified(Description),
    VanityNameInsert = null_if_unspecified(VanityName),
    TaglineInsert = null_if_unspecified(Tagline),
    DateFoundedInsert = null_if_unspecified(DateFounded),

    OrgInserts = [{IO, [OrgId, ParentInsert, Name, TaglineInsert, DescriptionInsert, VanityNameInsert, DateFoundedInsert, Visibility]}],
    {OrgInserts, OrgId}.


null_if_unspecified({}) -> null;
null_if_unspecified(<<"">>) -> null;
null_if_unspecified(Else) -> Else.

get_work_inserts(#work{title=Title,
                       authors=Authors},
                 State=#state{insert_work_statement=IW,
                              insert_authorship_statement=IA}) ->
    WorkUUID = fresh_uuid(),

    {PersonInserts, Ids} = get_people_inserts(Authors, State),
    AuthorshipInserts = lists:map(fun (AuthorUUID) ->
                                      {IA, [WorkUUID, AuthorUUID]}
                                  end, Ids),

    WorkInserts = lists:append([ [{IW, [WorkUUID, Title, <<"public">>]}],
                                 PersonInserts,
                                 AuthorshipInserts]),
    {WorkInserts, WorkUUID}.


get_people_inserts(PersonList, #state{insert_person_statement=IP}) ->
    PersonPairs = lists:map(fun ({Type, Value}) ->
                                  case Type of
                                      name -> 
                                          PersonUUID = fresh_uuid(),
                                          {{IP, [PersonUUID, Value]}, PersonUUID};
                                      id ->
                                          {none, Value}
                                  end
                              end,  PersonList),
    {Inserts, Ids} = lists:unzip(PersonPairs),
    Filtered = lists:filter(fun(X) -> X =/= none end, Inserts),
    {Filtered, Ids}.


fresh_uuid() ->
    uuid:to_string(uuid:uuid4()).


prepare_statements(C) ->
    {ok, BeginStmt} = epgsql:parse(C, "begin_statement", "BEGIN", []),
    {ok, CommitStmt} = epgsql:parse(C, "commit_statement", "COMMIT", []),


    %% INSERT Statements
    AuthorshipSql = "INSERT INTO authorship (work_id, person_id) VALUES($1, $2)",
    {ok, InsertAuthorship} = epgsql:parse(C, "insert_authorship", AuthorshipSql, [uuid, uuid]),

    PersonSql = "INSERT INTO people (person_id, name, photo_url, date_added) VALUES($1, $2, NULL, CURRENT_DATE)", 
    {ok, InsertPerson} = epgsql:parse(C, "insert_person", PersonSql, [uuid, text]),

    WorksSql = "INSERT INTO works (work_id, title, visibility) VALUES($1, $2, $3)", 
    {ok, InsertWork} = epgsql:parse(C, "insert_work", WorksSql, [uuid, text, text]),

    OrgsSql = "INSERT INTO organizations (org_id, parent_org, name, tagline, description, vanity_name, date_founded, visibility)"
        ++ " VALUES($1, $2, $3, $4, $5, $6, $7::date, $8)",
    {ok, InsertOrg} = epgsql:parse(C, "insert_organization", OrgsSql, [uuid, uuid, text, text, text, text, date, text]),

    PerformanceSql = "INSERT INTO performances (performance_id, work_id, show_id, director_id, performance_order)"
        ++ " VALUES($1, $2, $3, $4, $5)",
    {ok, InsertPerformance} = epgsql:parse(C, "insert_performance", PerformanceSql, [uuid, uuid, uuid, uuid, int4]),

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
    GetShowSql = "SELECT s.title, o.name, s.special_thanks, d.show_date "
        ++ "FROM shows AS s INNER JOIN organizations AS o ON (s.producing_org_id = o.org_id) "
        ++ "INNER JOIN show_dates AS d USING (show_id) WHERE s.show_id = $1",
    {ok, GetShow} = epgsql:parse(C, "get_show_meta", GetShowSql, [uuid]),


    %% This monstrosity will get all the performers in a show, even if spanning
    %% Many performances.
    GetOnstageSql = "SELECT w.title, ppl.name AS director_name, people2.name AS performer_name, po.role "
        ++ "FROM works AS w INNER JOIN performances AS p USING (work_id) "
        ++ "INNER JOIN people AS ppl ON (ppl.person_id = p.director_id) "
        ++ "INNER JOIN performance_onstage AS po USING (performance_id) "
        ++ "INNER JOIN people AS people2 ON (people2.person_id = po.performer_id) "
        ++ "WHERE p.show_id = $1 "
        ++ "GROUP BY p.performance_id, w.title, ppl.name, people2.name, po.role ORDER BY p.performance_order;",
    {ok, GetOnstage} = epgsql:parse(C, "get_show_onstage", GetOnstageSql, [uuid]),

    %% Pulls the authors of a show.
    GetAuthorsSql = "SELECT w.title, p.name AS author_name FROM "
        ++ "(SELECT p.work_id FROM performances AS p WHERE p.show_id = $1) AS works_in_show "
        ++ "INNER JOIN works AS w USING (work_id) INNER JOIN authorship AS a USING (work_id) "
        ++ "INNER JOIN people AS p USING (person_id)",
    {ok, GetAuthors} = epgsql:parse(C, "get_show_authors", GetAuthorsSql, [uuid]),

    State = #state{connection=C,
                   begin_statement=BeginStmt,
                   commit_statement=CommitStmt,

                   insert_work_statement=InsertWork,
                   insert_authorship_statement=InsertAuthorship,
                   insert_person_statement=InsertPerson,
                   insert_performance_statement=InsertPerformance,
                   insert_onstage_statement=InsertOnstage,
                   insert_offstage_statement=InsertOffstage,
                   insert_show_statement=InsertShow,
                   insert_dates_statement=InsertDates,
                   insert_org_statement=InsertOrg,

                   get_show_meta=GetShow,
                   get_show_onstage=GetOnstage,
                   get_show_authorship=GetAuthors
                   },
    State.
