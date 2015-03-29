%%% The DATABASE
%%%
%%% The primary value-prop of Ghostlight comes from its database. Virtually
%%% _everything_ that gives it value over a pile of playbills comes from the
%%% fact that a) we have a well-maintained, well-run database containing all
%%% the information, and b) that data is easily searchable, well-presented,
%%% and c) that the data is Good.
%%%
%%% While c) is mostly a policy issue (and some data science I don't know),
%%% and b) is the interface to the app itself, most of a) gets handled here!
%%%
%%% Here is the high-level on the database:
%%% * It exposes a number of API functions to the app that are mostly wrappers
%%%   to gen_server:call/cast. Technically speaking, we can change everything
%%%   about this module EXCEPT this -- we could change DB engines, ditch
%%%   gen_server, whatever. Highly unlikely, but that's the hard module
%%%   boundary for the rest of the app.
%%% * We make it a gen_server for the well-known obvious reasons: we can restart
%%%   it, supervise it, etc., and it's a solid way to maintain the state that
%%%   we'll need (parsed queries, PSQL connections).
%%% * We use Postgres because it's actually Free Free, unlike Neo4j, relies on
%%%   SQL which we and the world know, and is rock-rock-steady.
%%%
%%% Some ground rules for the module:
%%%
%%% * SQL is invisible. No evidence of the statements/queries or the rows and
%%%   row structure should escape this module. We trade entirely in the records
%%%   specified in ghostlight_data. Sometimes they're the same as input records,
%%%   sometimes they they're <datatype>_return for when we couple together
%%%   other data during a query.
%%%
%%% # Inserting New Data
%%%
%%% Since the data is so spread out with a myriad of hairy foreign key
%%% relationships, generating the exact queries for the use case requires something
%%% of a calculus. Virtually all data types have a get_<type>_inserts which takes a
%%% record from `ghostlight_data` and returns two-tuple -> a list of inserts, and
%%% the newly-generated ID of the entity being created. So:
%%%
%%% get_show_inserts(#show{}) -> {[Inserts], ShowId}
%%%   Insert :: {PreparedStmt, [Arg]}
%%%   ShowId :: UUID
%%%
%%% Here's where it gets interesting: given than a show contains performances, _it_
%%% will call get_performance_inserts, and bundle THOSE inserts into its own. So it
%%% really becomes something like Russian Nesting Dolls.

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
         insert_work/1,
         insert_org/1,
         insert_person/1,

         get_show/1,
         get_person/1,
         get_org/1,
         get_work/1,
        
         get_show_listings/0,
         get_work_listings/0,
         get_org_listings/0,
         get_person_listings/0]).

-export([fix_dups/0]).

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
                insert_director_statement,
                insert_onstage_statement,
                insert_offstage_statement,
                insert_show_statement,
                insert_dates_statement,

                get_show_meta,
                get_show_onstage,
                get_show_offstage,
                get_show_authorship,
                get_show_directors,

                get_show_listings,
                get_work_listings,
                get_org_listings,
                get_person_listings,

                get_person_name,
                get_person_authorship,
                get_person_orgs,
                get_person_onstage,
                get_person_offstage,
                get_person_directorships,

                get_org_meta,
                get_org_show_dates,
                get_produced_by_org,
                get_org_employees,

                get_work_meta,
                get_work_shows,

                person_de_dupes,
                org_de_dupes
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

handle_call({insert_work, Work}, _From, State) ->
    {Inserts, _Ids} = get_work_inserts(Work, State),
    Reply = exec_batch(Inserts, State),
    {reply, Reply, State};

handle_call({insert_org, Org}, _From, State) ->
    {Inserts, _Ids} = get_org_inserts(Org, State),
    Reply = exec_batch(Inserts, State),
    {reply, Reply, State};

handle_call({insert_person, Person}, _From, State) ->
    {Inserts, _Ids} = get_people_inserts([Person], State),
    Reply = exec_batch(Inserts, State),
    {reply, Reply, State};

%% As with insertions, querying a show is a pretty hairy endeavor since it touches
%% so many tables. There's probably some major SQL-foo I could employ to get 
%% almost exactly what I need.
%%
%% Here's the expectation from the DB on getting a show:
%%
%% * What is it? Work, org, dates.  TODO: Venue.
%% * Who was in it and who worked on it? Actors, directors, stage hands, etc.
%%
%% Later maybe we could do things like who went/who's going, or sorting by
%% shows that haven't closed yet.
handle_call({get_show, ShowId}, _From, State=#state{get_show_meta=SM,
                                                    get_show_onstage=SO,
                                                    get_show_offstage=SOff,
                                                    get_show_authorship=SA,
                                                    get_show_directors=SD}) ->
    Batch = [ {SM, [ShowId]},
              {SO, [ShowId]},
              {SOff, [ShowId]},
              {SA, [ShowId]},
              {SD, [ShowId]}],
    Reply = exec_batch(Batch, State),
    {reply, Reply, State};


handle_call(get_show_listings, _From, State=#state{connection=C, get_show_listings=GL}) ->
    epgsql:bind(C, GL, "", []),
    {ok, Rows} = epgsql:execute(C, GL),
    {reply, Rows, State};


%% In many ways, the person resource is the most important one -- we're
%% doing this, in part, to make something of a portfolio to see and
%% grow. You want to be proud when you see this resource. You want to feel
%% inspired to add to it.
%%
%% While there will probably be a lot more later on, here is a start for
%% what to return when looking up a person:
%%
%% * What is their name and other "person" data (i.e. a bio, photo urls, etc.)
%% * Where have been an onstage performer?
%% * Where have they been an offstage contributor?
%% * Have they directed any performances?
%% * Are they authors or creators of original work?
%% * Are they affiliated with organizations?
%%
%% Query should fetch all of this and make it presentable. Unfortunately,
%% until I beef up on Views or Postgres features, this is a batch of 5 queries,
%% 6 once I get directors. I'll see what I can do.
handle_call({get_person, PersonId}, _From, State=#state{get_person_name=GN,
                                                        get_person_authorship=GA,
                                                        get_person_orgs=GO,
                                                        get_person_directorships=GD,
                                                        get_person_onstage=POn,
                                                        get_person_offstage=POff}) ->

    Batch = [ {GN, [PersonId]},
              {GA, [PersonId]},
              {GO, [PersonId]},
              {GD, [PersonId]},
              {POn, [PersonId]},
              {POff, [PersonId]} ],
    Reply = exec_batch(Batch, State),

    {reply, Reply, State};


handle_call(get_person_listings, _From, State=#state{connection=C, get_person_listings=GP}) ->
    epgsql:bind(C, GP, "", []),
    {ok, Rows} = epgsql:execute(C, GP),
    {reply, Rows, State};


%% Orgs are pretty straightforward -- who's in them, and what have they
%% produced. Need to see if I care about in-app membership enough to 
%% make that happen here.
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


handle_call(get_org_listings, _From, State=#state{connection=C, get_org_listings=GO}) ->
    epgsql:bind(C, GO, "", []),
    {ok, Rows} = epgsql:execute(C, GO),
    {reply, Rows, State};


%% Works are also straightforward -- get their authors, and where they've been
%% produced.
handle_call({get_work, WorkId}, _From, State=#state{get_work_meta=GWM,
                                                    get_work_shows=GWS}) ->
    Batch = [ {GWM, [WorkId]},
              {GWS, [WorkId]} ],

    Reply = exec_batch(Batch, State),
    {reply, Reply, State};


handle_call(get_work_listings, _From, State=#state{connection=C, get_work_listings=GW}) ->
    epgsql:bind(C, GW, "", []),
    {ok, Rows} = epgsql:execute(C, GW),
    {reply, Rows, State};


handle_call(fix_dups, _From, State=#state{connection=C,
                                          begin_statement=BEGIN,
                                          commit_statement=COMMIT,
                                          person_de_dupes=PplDeDupes,
                                          org_de_dupes=OrgDeDupes}) ->
    {ok, _Columns, PeopleRows} = epgsql:squery(C, "SELECT id1, id2, name FROM (SELECT p1.person_id AS id1, p2.person_id AS id2, "
        ++ "p1.name, row_number() OVER (PARTITION BY p1.name) AS row_num FROM people AS p1, people AS p2 "
        ++ "WHERE p1.name = p2.name AND p1.person_id != p2.person_id ORDER BY p1.name) AS tmp WHERE row_num < 2"),

    lists:foreach(fun ({GoodId, BadId, Name}) ->
                     lager:info("Swapping out ~p for ~p for user ~p", [BadId, GoodId, Name]),
                     Statements = [ {Parsed, [GoodId, BadId]} || Parsed <- PplDeDupes ],
                     epgsql:execute_batch(C, lists:append([ [{BEGIN, []}], Statements, [{COMMIT, []}] ]))
                  end, PeopleRows),

    {ok, _Columns, OrgRows} = epgsql:squery(C, "SELECT id1, id2, name FROM (SELECT o1.org_id AS id1, o2.org_id AS id2, "
        ++ "o1.name, row_number() OVER (PARTITION BY o1.name) AS row_num FROM organizations AS o1, organizations AS o2 "
        ++ "WHERE o1.name = o2.name AND o1.org_id != o2.org_id ORDER BY o1.name) AS tmp WHERE row_num < 2"),

    lists:foreach(fun ({GoodId, BadId, Name}) ->
                     lager:info("Swapping out ~p for ~p for org ~p", [BadId, GoodId, Name]),
                     Statements = [ {Parsed, [GoodId, BadId]} || Parsed <- OrgDeDupes ],
                     epgsql:execute_batch(C, lists:append([ [{BEGIN, []}], Statements, [{COMMIT, []}] ]))
                  end, OrgRows),

    {reply, ok, State};


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

insert_work(Work) ->
    gen_server:call(?MODULE, {insert_work, Work}).

insert_org(Org) ->
    gen_server:call(?MODULE, {insert_org, Org}).

insert_person(Person) ->
    gen_server:call(?MODULE, {insert_person, Person}).

get_show_listings() ->
    Response = gen_server:call(?MODULE, get_show_listings),
% "SELECT s.show_id, s.title, w.work_id, w.title, p.person_id, p.name, o.org_id, o.name, d.show_date "

    [ #show{
         id=ShowId,
         title=ShowName,
         org=#organization{
                id=OrgId,
                name=OrgName
               }
        } || {ShowId, ShowName, OrgId, OrgName} <- Response ].



get_show(ShowId) ->
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
          onstage = maps:get({WorkId, Title}, PerformancesMap),
          offstage = maps:get({WorkId, Title}, OffstageMap),
          directors = maps:get(Title, DirectorMap)
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


get_person_listings() ->
    Response = gen_server:call(?MODULE, get_person_listings),
    [ #person{id=PersonId, name=PersonName} || {PersonId, PersonName} <- Response ].


get_person(PersonId) ->
    Response = gen_server:call(?MODULE, {get_person, PersonId}),
    [{ok, _}, {ok, [{Name}]}, {ok, AuthorshipList}, {ok, OrgsList}, {ok, DirectedList}, {ok, OnstageList}, {ok, OffstageList}, {ok, _}] = Response,
    Authors = [ #work{id=WorkId, title=Title } || {WorkId, Title, _Author} <- AuthorshipList],
    Orgs = [ #org_work{org_id=OrgId, org_name=OrgName, title=Job} || {OrgId, OrgName, Job} <- OrgsList],
    Onstage = [ #show{ title=ShowTitle,
                       id=ShowId,
                       org=#organization{id=OrgId, name=OrgName},
                       performances=[#performance{
                                       work=#work{ id=WorkId, title=WorkTitle },
                                       onstage=#onstage{ role=Role }
                                    }]
                     } || {ShowId, ShowTitle, WorkId, WorkTitle, OrgId, OrgName, Role} <- OnstageList ],
    Offstage = [ #show{ title=ShowTitle,
                       id=ShowId,
                       org=#organization{id=OrgId, name=OrgName},
                       performances=[#performance{
                                       work=#work{ id=WorkId, title=WorkTitle },
                                       offstage=#offstage{ job=Job }
                                    }]
                     } || {ShowId, ShowTitle, WorkId, WorkTitle, OrgId, OrgName, Job} <- OffstageList ],
    Directed = [ #show{ title=ShowTitle,
                        id=ShowId,
                        org=#organization{id=OrgId, name=OrgName},
                        performances=[#performance{
                                        work=#work{ id=WorkId, title=WorkTitle }
                                     }]
                      } || {ShowId, ShowTitle, WorkId, WorkTitle, OrgId, OrgName} <- DirectedList ],

    #person_return{
       id = PersonId,
       name = Name,
       authored = Authors,
       directed = Directed,
       onstage = Onstage,
       offstage = Offstage,
       orgs = Orgs
    }.


get_org_listings() ->
    Results = gen_server:call(?MODULE, get_org_listings),
    [ #organization{
         id=OrgId,
         name=OrgName,
         tagline=Tagline,
         description=Description
        } || {OrgId, OrgName, Tagline, Description} <- Results].


get_org(OrgId) ->
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



get_work_listings() ->
    Response = gen_server:call(?MODULE, get_work_listings),

    AuthorMap = lists:foldl(fun({Id, Title, AuthorId, AuthorName}, Accum) ->
                                Author = #person{
                                            id=AuthorId,
                                            name=AuthorName
                                         },
                                case maps:get({Id, Title}, Accum, undefined) of
                                    undefined ->
                                        maps:put({Id, Title}, [Author], Accum);
                                    AuthorList ->
                                        maps:put({Id, Title}, [Author|AuthorList], Accum)
                                end
                            end, maps:new(), Response),
    [ #work{
          id=WorkId,
          title=WorkTitle,
          authors=maps:get({WorkId, WorkTitle}, AuthorMap)
      } || {WorkId, WorkTitle} <- maps:keys(AuthorMap)].


%% Works return their list of authors, as well as when they were produced.
get_work(WorkId) ->
    Response = gen_server:call(?MODULE, {get_work, WorkId}),
    [{ok, []}, {ok, Authors}, {ok, Shows}, {ok, []}] = Response,
    [{WorkTitle, _, _, Description, MinutesLong}|_] = Authors,
    AuthorList = [ #person{ id = AuthorId, name = AuthorName } || {_, AuthorId, AuthorName, _, _} <- Authors ],
    ShowList = [ #show{
                     id=ShowId,
                     title=ShowTitle,
                     org=#organization{
                            id=OrgId,
                            name=OrgName
                         }
                 } || {ShowId, ShowTitle, OrgId, OrgName} <- Shows],
    #work_return{
       work=#work{
               id=WorkId,
               title=WorkTitle,
               authors=AuthorList,
               description=Description,
               minutes_long=MinutesLong
            },
       shows=ShowList
    }.

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
    {OrgInserts, OrgId} = get_org_inserts(Org, State),
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
    PerformanceInserts = [{IP, [PerformanceId, WorkId, ShowId, PerformanceOrder]}],
    DirectorInserts = get_director_inserts(PerformanceId, Directors, State),
    OnstageInserts = get_onstage_inserts(PerformanceId, Onstage, State),
    OffstageInserts = get_offstage_inserts(PerformanceId, Offstage, State),

    Inserts = lists:append([PerformanceInserts,
                            DirectorInserts,
                            OnstageInserts,
                            OffstageInserts]),
    {Inserts, PerformanceId}.


get_director_inserts(PerformanceId, Directors, State=#state{insert_director_statement=ID}) ->
    ListOfLists = lists:map(fun (Person) ->
                                {PersonInserts, [PersonId]} = get_people_inserts([Person], State),
                                DirectorInsert = {ID, [PerformanceId, PersonId]},
                                lists:reverse([DirectorInsert|PersonInserts])
                            end, Directors),
    lists:flatten(ListOfLists).

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
                                OffstageInsert = {IO, [PerformanceId, PersonId, Job, null, null]},
                                lists:reverse([OffstageInsert|PersonInserts])
                            end, OffstageList),
    lists:flatten(ListOfLists).


get_org_inserts(#organization {
                    name=Name,
                    tagline=Tagline,
                    description=Description,
                    parent=Parent,
                    vanity_name=VanityName,
                    date_founded=DateFounded,
                    visibility=Visibility
                }, #state{insert_org_statement=IO}) ->
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
                       authors=Authors,
                       description=Description,
                       minutes_long=MinutesLong},
                 State=#state{insert_work_statement=IW,
                              insert_authorship_statement=IA}) ->
    WorkUUID = fresh_uuid(),

    {PersonInserts, Ids} = get_people_inserts(Authors, State),
    AuthorshipInserts = lists:map(fun (AuthorUUID) ->
                                      {IA, [WorkUUID, AuthorUUID]}
                                  end, Ids),

    WorkInserts = lists:append([ [{IW, [WorkUUID, Title, Description, MinutesLong, <<"public">>]}],
                                 PersonInserts,
                                 AuthorshipInserts]),
    {WorkInserts, WorkUUID}.


get_people_inserts(PersonList, #state{insert_person_statement=IP}) ->
    PersonPairs = lists:map(fun (#person{id=PersonId, name=PersonName}) ->
                                  case PersonId of
                                      null -> 
                                          PersonUUID = fresh_uuid(),
                                          {{IP, [PersonUUID, PersonName]}, PersonUUID};
                                      Value ->
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

    WorksSql = "INSERT INTO works (work_id, title, description, minutes_long, acl) VALUES($1, $2, $3, $4, $5)", 
    {ok, InsertWork} = epgsql:parse(C, "insert_work", WorksSql, [uuid, text, text, int8, text]),

    OrgsSql = "INSERT INTO organizations (org_id, parent_org, name, tagline, description, vanity_name, date_founded, visibility)"
        ++ " VALUES($1, $2, $3, $4, $5, $6, $7::date, $8)",
    {ok, InsertOrg} = epgsql:parse(C, "insert_organization", OrgsSql, [uuid, uuid, text, text, text, text, date, text]),

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

    GetPersonNameSql = "SELECT name FROM people WHERE person_id = $1",
    {ok, GetPersonName} = epgsql:parse(C, "get_person_name", GetPersonNameSql, [uuid]),
    
    %% Pulls the works authored by a person.
    GetShowsAuthoredSql = "SELECT w.work_id, w.title, p.name FROM authorship AS a "
        ++ "INNER JOIN works AS w USING (work_id) INNER JOIN people AS p using (person_id) "
        ++ "WHERE w.acl = 'public' AND p.person_id = $1",
    {ok, GetShowsAuthored} = epgsql:parse(C, "get_authorships", GetShowsAuthoredSql, [uuid]),

    GetPersonOrgsSql = "SELECT o.org_id, o.name, oe.title FROM organizations AS o, org_employees AS oe "
        ++ "WHERE oe.person_id = $1 AND o.visibility = 'public'",
    {ok, GetPersonOrgs} = epgsql:parse(C, "get_person_orgs", GetPersonOrgsSql, [uuid]),

    GetPersonOnstageSql = "SELECT s.show_id, s.title, w.work_id, w.title, o.org_id, o.name, po.role FROM shows AS s " ++ "INNER JOIN performances AS p USING (show_id) INNER JOIN organizations AS o ON (o.org_id = s.producing_org_id) "
        ++ "INNER JOIN performance_onstage AS po USING (performance_id) INNER JOIN works AS w ON (p.work_id = w.work_id) "
        ++ "WHERE po.performer_id = $1",
    {ok, GetPersonOnstage} = epgsql:parse(C, "get_person_onstage", GetPersonOnstageSql, [uuid]),

    GetPersonOffstageSql = "SELECT s.show_id, s.title, w.work_id, w.title, o.org_id, o.name, po.job FROM shows AS s "
        ++ "INNER JOIN performances AS p USING (show_id) INNER JOIN organizations AS o ON (o.org_id = s.producing_org_id) "
        ++ "INNER JOIN performance_offstage AS po USING (performance_id) INNER JOIN works AS w ON (p.work_id = w.work_id) "
        ++ "WHERE po.person_id = $1",
    {ok, GetPersonOffstage} = epgsql:parse(C, "get_person_offstage", GetPersonOffstageSql, [uuid]),

    GetPersonDirectedSql = "SELECT s.show_id, s.title, w.work_id, w.title, o.org_id, o.name FROM shows AS s "
        ++ "INNER JOIN performances AS p USING (show_id) INNER JOIN organizations AS o ON (o.org_id = s.producing_org_id) "
        ++ "INNER JOIN performance_directors AS pd USING (performance_id) INNER JOIN works AS w ON (p.work_id = w.work_id) "
        ++ "WHERE pd.director_id = $1",
    {ok, GetPersonDirected} = epgsql:parse(C, "get_person_directed", GetPersonDirectedSql, [uuid]),

    GetPersonListingsSql = "SELECT p.person_id, p.name FROM people AS p ORDER BY p.name ASC",
    {ok, GetPersonListings} = epgsql:parse(C, "get_person_listings", GetPersonListingsSql, []),


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



    GetWorkTitleAndAuthorsSql = "SELECT w.title, p.person_id, p.name, w.description, w.minutes_long FROM works AS w INNER JOIN authorship AS a USING (work_id) "
        ++ "INNER JOIN people AS p USING (person_id) WHERE a.work_id = $1",
    {ok, GetWorkTitleAndAuthors} = epgsql:parse(C, "get_work_meta", GetWorkTitleAndAuthorsSql, [uuid]),

    GetWorkShowsSql = "SELECT s.show_id, s.title, o.org_id, o.name AS org_name FROM shows AS s "
        ++ "INNER JOIN performances AS p USING (show_id) INNER JOIN organizations AS o ON (o.org_id = s.producing_org_id) "
        ++ "INNER JOIN works AS w ON (p.work_id = w.work_id) WHERE w.work_id = $1",

        
    %% For work listings -- much like the meta of a single one.
    GetWorkListingsSql = "SELECT w.work_id, w.title, p.person_id, p.name FROM works AS w "
        ++ "INNER JOIN authorship AS a using (work_id) INNER JOIN people AS p USING (person_id) ORDER BY w.title ASC LIMIT 50",
    {ok, GetWorkListings} = epgsql:parse(C, "get_work_listings", GetWorkListingsSql, []),



    {ok, GetWorkShows} = epgsql:parse(C, "get_work_shows", GetWorkShowsSql, [uuid]),
                
    %% De-duping
    {ok, Parsed1} = epgsql:parse(C, "consolidate_1", "UPDATE authorship SET person_id = $1 WHERE person_id = $2", [uuid, uuid]),
    {ok, Parsed2} = epgsql:parse(C, "consolidate_2", "UPDATE org_employees SET person_id = $1 WHERE person_id = $2", [uuid, uuid]),
    {ok, Parsed3} = epgsql:parse(C, "consolidate_3", "UPDATE performance_offstage SET person_id = $1 WHERE person_id = $2", [uuid, uuid]),
    {ok, Parsed4} = epgsql:parse(C, "consolidate_4", "UPDATE performance_directors SET director_id = $1 WHERE director_id = $2", [uuid, uuid]),
    {ok, Parsed5} = epgsql:parse(C, "consolidate_5", "UPDATE performance_onstage SET performer_id = $1 WHERE performer_id = $2", [uuid, uuid]),
    {ok, Parsed6} = epgsql:parse(C, "consolidate_7", "UPDATE users SET person_id = $1 WHERE person_id = $2", [uuid, uuid]),
    {ok, Parsed7} = epgsql:parse(C, "consolidate_8", "DELETE FROM people WHERE person_id = $2", [uuid, uuid]),
    PplDeDupes = [Parsed1, Parsed2, Parsed3, Parsed4, Parsed5, Parsed6, Parsed7],

    {ok, OrgParsed1} = epgsql:parse(C, "consolidate_a", "UPDATE org_employees SET org_id = $1 WHERE org_id = $2", [uuid, uuid]),
    {ok, OrgParsed2} = epgsql:parse(C, "consolidate_b", "UPDATE org_memberships SET org_id = $1 WHERE org_id = $2", [uuid, uuid]),
    {ok, OrgParsed3} = epgsql:parse(C, "consolidate_c", "UPDATE shows SET producing_org_id = $1 WHERE producing_org_id = $2", [uuid, uuid]),
    {ok, OrgParsed4} = epgsql:parse(C, "consolidate_d", "DELETE FROM organizations WHERE org_id = $2", [uuid, uuid]),
    OrgDeDupes = [OrgParsed1, OrgParsed2, OrgParsed3, OrgParsed4],
 
    State = #state{connection=C,
                   begin_statement=BeginStmt,
                   commit_statement=CommitStmt,

                   insert_work_statement=InsertWork,
                   insert_authorship_statement=InsertAuthorship,
                   insert_person_statement=InsertPerson,
                   insert_performance_statement=InsertPerformance,
                   insert_director_statement=InsertDirector,
                   insert_onstage_statement=InsertOnstage,
                   insert_offstage_statement=InsertOffstage,
                   insert_show_statement=InsertShow,
                   insert_dates_statement=InsertDates,
                   insert_org_statement=InsertOrg,

                   get_show_meta=GetShow,
                   get_show_onstage=GetOnstage,
                   get_show_offstage=GetOffstage,
                   get_show_authorship=GetAuthors,
                   get_show_directors=GetDirectors,

                   get_show_listings=GetShowListings,
                   get_work_listings=GetWorkListings,
                   get_org_listings=GetOrgListings,
                   get_person_listings=GetPersonListings,

                   get_person_name=GetPersonName,
                   get_person_authorship=GetShowsAuthored,
                   get_person_orgs=GetPersonOrgs,
                   get_person_onstage=GetPersonOnstage,
                   get_person_offstage=GetPersonOffstage,
                   get_person_directorships=GetPersonDirected,

                   get_org_meta=GetOrgMeta,
                   get_produced_by_org=GetProducedByOrg,
                   get_org_show_dates=GetDatesOfShow,
                   get_org_employees=GetOrgEmployees,

                   get_work_meta=GetWorkTitleAndAuthors,
                   get_work_shows=GetWorkShows,

                   person_de_dupes=PplDeDupes,
                   org_de_dupes=OrgDeDupes
                   },
    State.

%%% SUPER SECRET FUNCTIONS
fix_dups() ->
    gen_server:call(?MODULE, fix_dups).
