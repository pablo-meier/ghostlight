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
                                                       get_show_producers=SProd,
                                                    get_show_onstage=SO,
                                                    get_show_offstage=SOff,
                                                    get_show_authorship=SA,
                                                    get_show_directors=SD,
                                                    get_show_links=SL,
                                                    get_show_press=SP,
                                                    get_show_hosts=SH }) ->
    Batch = [ {SM, [ShowId]},
              {SProd, [ShowId]},
              {SO, [ShowId]},
              {SOff, [ShowId]},
              {SA, [ShowId]},
              {SD, [ShowId]},
              {SP, [ShowId]},
              {SL, [ShowId]},
              {SH, [ShowId]}],
    Reply = ghostlight_db_utils:exec_batch(Batch, State),
    {reply, Reply, State};

handle_call({get_inserts, Show}, _From, State) ->
    get_inserts(Show, State);

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

get_inserts(#show{title=Title,
                  producers=Producers,
                  performances=Performances,
                  special_thanks=SpecialThanks,
                  dates=Dates,
                  hosts=Hosts,
                  press_links=PressLinks,
                  external_links=ExternalLinks
            },
            State=#db_state{insert_show_statement=IS,
                            insert_dates_statement=ID,
                            insert_links_statement=IL,
                            insert_presslinks_statement=IP}) ->
    Works = extract_works(Performances),
    {AllWorkInserts, WorksWithId} = fold_over_works(Works),

    ShowId = ghostlight_db_utils:fresh_uuid(),

    {ProducerInsertsRaw, _ProducerIds} = lists:unzip([ get_producer_inserts(ShowId, Producer, Num, State) 
                                                       || {Producer, Num} <- lists:zip(Producers, lists:seq(1, length(Producers))) ]),
    ProducerInserts = lists:flatten(ProducerInsertsRaw),

    ShowInserts = [{IS, [ShowId, Title, SpecialThanks]}],
    DateInserts = lists:map(fun (Date) ->
                                {ID, [ShowId, Date]}
                            end, Dates),
    AllPerformanceInserts = fold_over_performances(Performances, ShowId, WorksWithId, State),

    HostInserts = get_host_inserts(ShowId, Hosts, State),
    PressInserts = [ {IP, [ShowId, Link, Description]} || #press_link{link=Link, description=Description} <- PressLinks],
    LinkInserts = ghostlight_db_utils:external_links_inserts(ShowId, IL, ExternalLinks),

    Batch = lists:append([
                          AllWorkInserts,
                          ShowInserts,
                          ProducerInserts,
                          DateInserts,
                          AllPerformanceInserts,
                          HostInserts,
                          LinkInserts,
                          PressInserts
                         ]),
    {Batch, ShowId}.


get_producer_inserts(ShowId, Person=#person{}, Order, #db_state{insert_producer_statement=IP}) ->
    {PersonInserts, PersonId} = ghostlight_db_person:get_inserts(Person),
    WithProduction = lists:append([ PersonInserts,
                                    [{IP, [ShowId, null, PersonId, Order]}] ]),
    {WithProduction, PersonId};
get_producer_inserts(ShowId, Org=#organization{}, Order, #db_state{insert_producer_statement=IP}) ->
    {OrgInserts, OrgId} = ghostlight_db_org:get_inserts(Org),
    WithProduction = lists:append([ OrgInserts,
                                    [{IP, [ShowId, OrgId, null, Order]}] ]),
    {WithProduction, OrgId}.


extract_works(Performances) ->
    lists:map(fun (#performance{work = Work}) -> Work end, Performances).

get_performance_inserts(WorksWithIds,
                        ShowId,
                        PerformanceOrder,
                        #performance{ 
                           work=Work,
                           onstage=Onstage,
                           offstage=Offstage,
                           directors=Directors,
                           directors_note=DirectorNote,
                           description=Description
                        },
                        State=#db_state{insert_performance_statement=IP}) ->
    PerformanceId = ghostlight_db_utils:fresh_uuid(),
    WorkId = proplists:get_value(Work, WorksWithIds),

    DescMarkdown = ghostlight_db_utils:markdown_or_null(Description),
    DirNoteMarkdown = ghostlight_db_utils:markdown_or_null(DirectorNote),
    PerformanceInserts = [{IP, [PerformanceId, WorkId, ShowId, DirectorNote, DirNoteMarkdown, Description, DescMarkdown, PerformanceOrder]}],

    DirectorInserts = get_director_inserts(PerformanceId, Directors, State),
    OnstageInserts = get_onstage_inserts(PerformanceId, Onstage, State),
    OffstageInserts = get_offstage_inserts(PerformanceId, Offstage, State),

    Inserts = lists:append([PerformanceInserts,
                            DirectorInserts,
                            OnstageInserts,
                            OffstageInserts]),
    {Inserts, PerformanceId}.


get_host_inserts(ShowId, Hosts, #db_state{insert_hosts_statement=IH}) ->
    ListOfLists = lists:map(fun (Person) ->
                                {PersonInserts, PersonId} = ghostlight_db_person:get_inserts(Person),
                                DirectorInsert = {IH, [ShowId, PersonId]},
                                [PersonInserts, DirectorInsert]
                            end, Hosts),
    lists:flatten(ListOfLists).

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
    [_,
     {ok, Meta},
     {ok, Producers},
     {ok, Onstage},
     {ok, Offstage},
     {ok, Authorship},
     {ok, Directors},
     {ok, Press},
     {ok, External},
     {ok, Hosts},
     _] = Response,
    [{Title, Description, SpecialThanks, _}|_] = Meta,
    Dates = [ Date || {_, _, _, _, Date} <- Meta ],

    Authors = authors_to_map(Authorship),
    DirectorMap = directors_to_map(Directors),
    ExternalLinks = ghostlight_db_utils:external_links_sql_to_record(External),
    PressLinks = [ #press_link{link=Link, description=PressDesc} || {Link, PressDesc} <- Press ],

    #show{
        title = Title,
        special_thanks = SpecialThanks,
        dates = Dates,
        description = Description,
        hosts = [ #person{id=HostId, name=HostName} || {HostId, HostName} <- Hosts],
        producers=[ parse_producer_row(Producer) || Producer <- Producers ],
        performances = make_performance_record_list(Onstage, Offstage, Authors, DirectorMap),
        external_links=ExternalLinks,
        press_links=PressLinks
      }.

parse_producer_row({null, null, PersonId, PersonName}) ->
    #person{ id=PersonId, name=PersonName };
parse_producer_row({OrgId, OrgName, null, null}) ->
    #organization{ id=OrgId, name=OrgName }.

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

    %% This is a monstrosity and I'm sorry
    %% This is the final list of the elements, but they are ordered incorrectly due to how we
    %% built them in the first place. So I'm wrapping it in a map, and using the performance
    %% order of the OnstageList, where we ORDER BYed.
    ByName = lists:foldl(fun ({WorkId, Title}, Accum) ->
                             maps:put({WorkId, Title}, 
                                       #performance{
                                           work = #work {
                                                      id = WorkId,
                                                      title = Title,
                                                      authors = maps:get(Title, AuthorMap)
                                                  },
                                           onstage = maps:get({WorkId, Title}, PerformancesMap, []),
                                           offstage = maps:get({WorkId, Title}, OffstageMap, []),
                                           directors = element(1, maps:get(Title, DirectorMap, {[], null, null})),
                                           directors_note = element(2, maps:get(Title, DirectorMap, {[], null, null})),
                                           description = element(3, maps:get(Title, DirectorMap, {[], null, null}))
                                       }, Accum)
                         end, maps:new(), maps:keys(PerformancesMap)),

    %% This removes duplicate rows.
    ReversedKeys = lists:foldl(fun({WorkId, Title, _, _, _}, Accum) ->
                                   case Accum of 
                                       [] -> [{WorkId, Title}];
                                       [{WorkId,Title}|_] -> Accum;
                                       _Else -> [{WorkId, Title}|Accum]
                                   end
                               end, [], Onstage),

    [ maps:get({WorkId, Title}, ByName) ||  {WorkId, Title} <- lists:reverse(ReversedKeys)].


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

%% Similar to above, but with the description/director's note.
directors_to_map(AuthorList) ->
    lists:foldl(fun ({Title, AuthorId, Author, Description, DirectorsNote}, Accum) ->
            AuthorTuple = #person{id=AuthorId, name=Author},
            case maps:get(Title, Accum, none) of
                none ->
                    maps:put(Title, {[AuthorTuple], Description, DirectorsNote}, Accum);
                {Authors, D, DN} ->
                    maps:put(Title, {[AuthorTuple|Authors], D, DN}, Accum)
            end
        end, maps:new(), AuthorList).

listings() ->
    Response = gen_server:call(?MODULE, get_show_listings),
    [ #show{
         id=ShowId,
         title=ShowName
%%         org=#organization{
%%                id=OrgId,
%%                name=OrgName
%%               }
        } || {ShowId, ShowName, _OrgId, _OrgName} <- Response ].

insert(Show) ->
    gen_server:call(?MODULE, {insert_show, Show}).

get_inserts(Show) ->
    gen_server:call(?MODULE, {get_inserts, Show}).


prepare_statements(C, State) ->
    PerformanceSql = "INSERT INTO performances "
        ++ "(performance_id, work_id, show_id, directors_note_src, directors_note_markdown, description_src, description_markdown, performance_order)"
        ++ " VALUES($1, $2, $3, $4, $5, $6, $7, $8)",
    {ok, InsertPerformance} = epgsql:parse(C, "insert_performance", PerformanceSql, [uuid, uuid, uuid, text, text, text, text, int4]),

    DirectorSql = "INSERT INTO performance_directors (performance_id, director_id) VALUES($1, $2)",
    {ok, InsertDirector} = epgsql:parse(C, "insert_performance_director", DirectorSql, [uuid, uuid]),

    OnstageSql = "INSERT INTO performance_onstage (performance_id, performer_id, role, understudy_id, date_started, date_ended)"
        ++ " VALUES($1, $2, $3, $4, $5, $6)",
    {ok, InsertOnstage} = epgsql:parse(C, "insert_performance_onstage", OnstageSql, [uuid, uuid, text, uuid, date, date]),

    OffstageSql = "INSERT INTO performance_offstage (performance_id, person_id, job, date_started, date_ended)"
        ++ " VALUES($1, $2, $3, $4, $5)",
    {ok, InsertOffstage} = epgsql:parse(C, "insert_performance_offstage", OffstageSql, [uuid, uuid, text, date, date]),

    ProducersSql = "INSERT INTO producers (show_id, org_id, person_id, listed_order) VALUES($1, $2, $3, $4)",
    {ok, InsertProducer} = epgsql:parse(C, "insert_producer", ProducersSql, [uuid, uuid, uuid, int4]),

    ShowSql = "INSERT INTO shows (show_id, title, special_thanks, date_created) "
        ++ " VALUES($1, $2, $3, CURRENT_DATE)",
    {ok, InsertShow} = epgsql:parse(C, "insert_show", ShowSql, [uuid, text, text]),

    DatesSql = "INSERT INTO show_dates (show_id, show_date) VALUES($1, $2)",
    {ok, InsertDates} = epgsql:parse(C, "insert_show_dates", DatesSql, [uuid, timestamptz]),

    HostsSql = "INSERT INTO show_hosts (show_id, person_id) VALUES($1, $2)",
    {ok, InsertHost} = epgsql:parse(C, "insert_show_host", HostsSql, [uuid, uuid]),

    InsertLinkSql = "INSERT INTO show_links (show_id, link, type) VALUES($1, $2, $3::link_type)",
    {ok, InsertLink} = epgsql:parse(C, "insert_show_link", InsertLinkSql, [uuid, text, text]),

    InsertPressSql = "INSERT INTO press_links (show_id, link, description) VALUES($1, $2, $3)",
    {ok, InsertPress} = epgsql:parse(C, "insert_show_press", InsertPressSql, [uuid, text, text]),

    %% SELECT Statements
    %% This will get the show's metadata
    GetShowSql = "SELECT s.title, s.description_markdown, s.special_thanks, d.show_date "
        ++ "FROM shows AS s INNER JOIN show_dates AS d USING (show_id) WHERE s.show_id = $1",
    {ok, GetShow} = epgsql:parse(C, "get_show_meta", GetShowSql, [uuid]),

    GetProducersSql = "SELECT o.org_id, o.name, p.person_id, p.name FROM producers AS prod "
        ++ "LEFT OUTER JOIN organizations AS o USING (org_id) "
        ++ "LEFT OUTER JOIN people AS p USING (person_id) "
        ++ "WHERE prod.show_id = $1 ORDER BY prod.listed_order ASC",
    {ok, GetProducers} = epgsql:parse(C, "get_producers", GetProducersSql, [uuid]),

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
    GetDirectorsSql = "SELECT w.title, p.person_id AS director_id, p.name AS director_name, "
        ++ " perf.directors_note_markdown, perf.description_markdown FROM "
        ++ "works AS w INNER JOIN performances AS perf USING (work_id) INNER JOIN "
        ++ "performance_directors AS pd USING (performance_id) INNER JOIN people AS p "
        ++ "ON (p.person_id = pd.director_id) INNER JOIN shows AS s ON (perf.show_id = s.show_id) "
        ++ "WHERE s.show_id = $1",
    {ok, GetDirectors} = epgsql:parse(C, "get_show_directors", GetDirectorsSql, [uuid]),

    %% Get the shows links. 
    GetLinksSql = "SELECT link, type FROM show_links WHERE show_id = $1",
    {ok, GetLinks} = epgsql:parse(C, "get_show_links", GetLinksSql, [uuid]),

    %% Get the shows press.
    GetPressSql = "SELECT link, description FROM press_links WHERE show_id = $1",
    {ok, GetPress} = epgsql:parse(C, "get_show_press", GetPressSql, [uuid]),
    
    %% Get the shows press.
    GetHostsSql = "SELECT p.person_id, p.name FROM people AS p INNER JOIN show_hosts AS sh USING (person_id) WHERE sh.show_id = $1",
    {ok, GetHosts} = epgsql:parse(C, "get_show_hosts", GetHostsSql, [uuid]),
  
    %% For show listings -- much like the meta of a single one.
    GetShowListingsSql = "SELECT s.show_id, s.title FROM shows AS s LIMIT 30",
    {ok, GetShowListings} = epgsql:parse(C, "show_listings_meta", GetShowListingsSql, []),


 
    State#db_state{
       insert_performance_statement=InsertPerformance,
       insert_director_statement=InsertDirector,
       insert_onstage_statement=InsertOnstage,
       insert_offstage_statement=InsertOffstage,
       insert_show_statement=InsertShow,
       insert_dates_statement=InsertDates,
       insert_hosts_statement=InsertHost,
       insert_links_statement=InsertLink,
       insert_presslinks_statement=InsertPress,
       insert_producer_statement=InsertProducer,
       
       get_show_listings=GetShowListings,
       get_show_meta=GetShow,
       get_show_producers=GetProducers,
       get_show_onstage=GetOnstage,
       get_show_offstage=GetOffstage,
       get_show_authorship=GetAuthors,
       get_show_directors=GetDirectors,
       get_show_links=GetLinks,
       get_show_press=GetPress,
       get_show_hosts=GetHosts
     }.

