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
-module(ghostlight_db).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([insert_show/1]).

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
                insert_dates_statement
                }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, C} = epgsql:connect("localhost", "pablo", "", [{database, "ghostlight-dev"}]),
    initialize_tables(C),
    {ok, BeginStmt} = epgsql:parse(C, "begin_statement", "BEGIN", []),
    {ok, CommitStmt} = epgsql:parse(C, "commit_statement", "COMMIT", []),
    {ok, InsertAuthorship} = epgsql:parse(C,
                                          "insert_authorship",
                                          "INSERT INTO authorship (work_id, person_id) VALUES($1, $2)",
                                          [uuid, uuid]),
    {ok, InsertPerson} = epgsql:parse(C,
                                      "insert_person",
                                      "INSERT INTO people (person_id, name, photo_url, date_added) VALUES($1, $2, NULL, CURRENT_DATE)",
                                      [uuid, text]),

    {ok, InsertWork} = epgsql:parse(C,
                                    "insert_work",
                                    "INSERT INTO works (work_id, title, visibility) VALUES($1, $2, $3)",
                                    [uuid, text, text]),

    {ok, InsertOrg} = epgsql:parse(C,
                                   "insert_organization",
                                   "INSERT INTO organizations (org_id, parent_org, name, tagline, description, vanity_name, date_founded, visibility)"
                                    ++ " VALUES($1, $2, $3, $4, $5, $6, $7::date, $8)",
                                   [uuid, uuid, text, text, text, text, date, text]),

    {ok, InsertPerformance} = epgsql:parse(C,
                                           "insert_performance",
                                           "INSERT INTO performances (performance_id, work_id, show_id, director_id, performance_order)"
                                            ++ " VALUES($1, $2, $3, $4, $5)",
                                          [uuid, uuid, uuid, uuid, int4]),
    {ok, InsertOnstage} = epgsql:parse(C,
                                       "insert_performance_onstage",
                                       "INSERT INTO performance_onstage (performance_id, performer_id, role, understudy_id, date_started, date_ended)"
                                        ++ " VALUES($1, $2, $3, $4, $5, $6)",
                                       [uuid, uuid, text, uuid, date, date]),
    {ok, InsertOffstage} = epgsql:parse(C,
                                        "insert_performance_offstage",
                                        "INSERT INTO performance_offstage (performance_id, person_id, job, date_started, date_ended)"
                                        ++ " VALUES($1, $2, $3, $4, $5)",
                                       [uuid, uuid, text, date, date]),
    {ok, InsertShow} = epgsql:parse(C,
                                    "insert_show",
                                    "INSERT INTO shows (show_id, title, producing_org_id, special_thanks, date_created) "
                                    ++ " VALUES($1, $2, $3, $4, CURRENT_DATE)",
                                   [uuid, text, uuid, text]),
    {ok, InsertDates} = epgsql:parse(C,
                                     "insert_show_dates",
                                     "INSERT INTO show_dates (show_id, show_date) VALUES($1, $2)",
                                     [uuid, timestamptz]),
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
                   insert_org_statement=InsertOrg
                   },
    {ok, State}.

handle_call({insert_show, Show}, _From, State) ->
    Inserts = get_show_inserts(Show, State),
    Reply = exec_batch(Inserts, State),
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
    DateInserts = lists:map(fun (#ghostlight_datetime{time8601=Timestamp}) ->
                                {ID, [ShowId, iso8601:parse(Timestamp)]}
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


null_if_unspecified(#ghostlight_datetime{}) -> null;
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
