-module(ghostlight_db_show).

-export([
         get_statement/1,
         db_to_record/2,
         get_inserts/2,
         listings_statement/1,
         db_listings_to_record_list/1,
         prepare_statements/2
        ]).


-include("apps/ghostlight/include/ghostlight_data.hrl").


get_statement(#db_state{get_show_statement=GS}) ->
    GS.

listings_statement(#db_state{get_show_listings=GSL}) ->
    GSL.

db_to_record(
    [_,
     {ok, 
      [{ShowId,
        Title,
        Description,
        SpecialThanks,
        Producers,
        Press,
        External,
        Dates,
        Hosts,
        Performances}]},
     _],
     _Format) ->

    #show{
        id = ShowId,
        title = Title,
        special_thanks = SpecialThanks,
        dates = [ iso8601:parse(Date) || Date <- jiffy:decode(Dates) ],
        description = Description,
        hosts = [ #person{id=HostId, name=HostName} || {HostId, HostName} <- jiffy:decode(Hosts) ],
        producers=[ ghostlight_db_utils:parse_person_or_org(Producer) 
                    || Producer <- jiffy:decode(Producers) ],
        performances = [ parse_performance(Performance) || Performance <- jiffy:decode(Performances) ],
        external_links=ghostlight_db_utils:external_links_sql_to_record(jiffy:decode(External)),
        press_links=format_press_links(jiffy:decode(Press))
      }.

 
db_listings_to_record_list(Results) ->
    [ #show{
         id=ShowId,
         title=ShowTitle,
         description=ShowDescription,
         producers=[ ghostlight_db_utils:parse_person_or_org(Producer) 
                    || Producer <- jiffy:decode(ShowProducers) ],
         dates = [ iso8601:parse(Date) || Date <- jiffy:decode(ShowDates) ],
         performances=[ performance_from_work_json(Work)
                        || Work <- jiffy:decode(WorksJson) ]
        }
     || {
        ShowId,
        ShowTitle,
        ShowDescription,
        ShowProducers,
        ShowDates,
        WorksJson,
        _OpeningNight
     } <- Results ].


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
                                            contributor=Contrib}) ->
                                case Contrib of
                                    #person{} ->
                                        {PersonInserts, PersonId} = ghostlight_db_person:get_inserts(Contrib),
                                        OffstageInsert = {IO, [PerformanceId, PersonId, null, Job, null, null]},
                                        [PersonInserts, OffstageInsert];
                                    #organization{} ->
                                        {OrgInserts, OrgId} = ghostlight_db_org:get_inserts(Contrib),
                                        OffstageInsert = {IO, [PerformanceId, null, OrgId, Job, null, null]},
                                        [OrgInserts, OffstageInsert]
                                end
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

format_press_links(PressLinks) ->
    [ #press_link{
         link = proplists:get_value(<<"link">>, Unwrapped) ,
         description = proplists:get_value(<<"description">>, Unwrapped)
      } || {Unwrapped} <- PressLinks ].

parse_performance({Proplist}) ->
    #performance{
       work = #work{
                 id = proplists:get_value(<<"work_id">>, Proplist),
                 title = proplists:get_value(<<"work_title">>, Proplist),
                 authors = [ ghostlight_db_utils:parse_person_or_org(Author) 
                             || Author <- proplists:get_value(<<"authors">>, Proplist) ]
              },
       directors = [ parse_person(Director) || Director <- proplists:get_value(<<"directors">>, Proplist, []) ],
       onstage = [ parse_onstage(Onstage) || Onstage <- proplists:get_value(<<"onstage">>, Proplist, []) ],
       offstage = [ parse_offstage(Offstage) || Offstage <- proplists:get_value(<<"offstage">>, Proplist, []) ],
       directors_note = proplists:get_value(<<"directors_note">>, Proplist, []),
       description = proplists:get_value(<<"description">>, Proplist, [])
    }.

parse_onstage({Onstage}) ->
    Person = parse_person(proplists:get_value(<<"performer">>, Onstage)),
    Role = proplists:get_value(<<"role">>, Onstage),
    #onstage{ role=Role, person=Person }.
 
parse_offstage({Offstage}) ->
    Entity = ghostlight_db_utils:parse_person_or_org(proplists:get_value(<<"entity">>, Offstage)),
    Job = proplists:get_value(<<"job">>, Offstage),
    #offstage{ contributor=Entity, job=Job}.

parse_person({Person}) ->
    #person{
       id = proplists:get_value(<<"id">>, Person),
       name = proplists:get_value(<<"name">>, Person)
    }.


performance_from_work_json({WorkJson}) ->
    Id = proplists:get_value(<<"work_id">>, WorkJson),
    Title = proplists:get_value(<<"title">>, WorkJson),
    Authors = proplists:get_value(<<"authors">>, WorkJson),
    #performance{
       work=#work{
               id=Id,
               title=Title,
               authors=[ author_from_json(Author) || Author <- Authors ]
              }
    }.

%% Very odd, but this row in an object keyed on 'row'? FIXME.
author_from_json({Author}) ->
    {Info} = proplists:get_value(<<"row">>, Author),
    case proplists:get_value(<<"type">>, Info) of
        <<"person">> ->
            #person{
                id=proplists:get_value(<<"id">>, Info),
                name=proplists:get_value(<<"name">>, Info)
              };
        <<"org">> ->
            #organization {
                id=proplists:get_value(<<"id">>, Info),
                name=proplists:get_value(<<"name">>, Info)
            }
    end.



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

    OffstageSql = "INSERT INTO performance_offstage (performance_id, person_id, org_id, job, date_started, date_ended)"
        ++ " VALUES($1, $2, $3, $4, $5, $6)",
    {ok, InsertOffstage} = epgsql:parse(C, "insert_performance_offstage", OffstageSql, [uuid, uuid, uuid, text, date, date]),

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
    %% "BEHOLD," he cackled, "MY GREATEST CREATION!"
    GetShowSql = 
"
SELECT
    s.show_id,
    s.title,
    s.description_markdown,
    s.special_thanks,
    array_to_json(ARRAY(SELECT (CASE WHEN prod.person_id IS NULL
                      THEN ('org'::person_or_org_label, prod.org_id, o.name)::person_or_org
                      ELSE ('person'::person_or_org_label, prod.person_id, p.name)::person_or_org
                 END)
             FROM producers prod
             LEFT OUTER JOIN people p USING (person_id)
             LEFT OUTER JOIN organizations o USING (org_id)
             WHERE prod.show_id = s.show_id ORDER BY prod.listed_order ASC)) AS producers,
    array_to_json(ARRAY(SELECT (pl.link, pl.description)::press_link FROM press_links pl WHERE show_id = s.show_id)) AS press_links,
    array_to_json(ARRAY(SELECT (sl.link, sl.type)::external_link FROM show_links sl WHERE sl.show_id = s.show_id)) AS external_links,
    array_to_json(ARRAY(SELECT sd.show_date from show_dates sd WHERE sd.show_id = s.show_id ORDER BY sd.show_date ASC)) AS dates,
    array_to_json(ARRAY(SELECT (p.person_id, p.name)::person_pair FROM people p INNER JOIN show_hosts sh USING (person_id) where sh.show_id = s.show_id)) AS hosts,
    array_to_json(ARRAY(SELECT (perf.performance_id,
          w.work_id, 
          w.title, 
          ARRAY(SELECT (CASE WHEN a.person_id IS NULL 
                            THEN ('org'::person_or_org_label, a.org_id, o.name)::person_or_org
                            ELSE ('person'::person_or_org_label, a.person_id, p.name)::person_or_org
                       END)
                FROM authorship a
                LEFT OUTER JOIN people p USING (person_id)
                LEFT OUTER JOIN organizations o USING (org_id)
                WHERE a.work_id = w.work_id),
          perf.description_markdown,
          perf.directors_note_markdown,
          ARRAY(SELECT (p.person_id, p.name)::person_pair
                    FROM performance_directors pd
                    INNER JOIN people p ON (pd.director_id = p.person_id)
                    WHERE pd.performance_id = perf.performance_id),
          ARRAY(SELECT ((p.person_id, p.name)::person_pair, po.role)::onstage_performance
                    FROM performance_onstage po
                    INNER JOIN people p ON (p.person_id = po.performer_id)
                    WHERE po.performance_id = perf.performance_id),
          ARRAY(SELECT (CASE WHEN po.person_id IS NULL
                            THEN (('org'::person_or_org_label, po.org_id, o.name)::person_or_org, po.job)::offstage_performance
                            ELSE (('person'::person_or_org_label, po.person_id, p.name)::person_or_org, po.job)::offstage_performance
                        END)
                  FROM performance_offstage po
                  LEFT OUTER JOIN organizations o USING (org_id)
                  LEFT OUTER JOIN people p USING (person_id)
                  WHERE po.performance_id = perf.performance_id))::aggregated_performance
         FROM works w
         INNER JOIN performances perf USING (work_id)
         WHERE perf.show_id = s.show_id ORDER BY perf.performance_order ASC)) AS performances
FROM shows AS s where s.show_id = $1",
    {ok, GetShowStatement} = epgsql:parse(C, "get_show_statement", GetShowSql, [uuid]),
  
    %% Show listings -- get the id title, description, works presented, producers, run, ordered by date.
    GetShowListingsSql =
"
SELECT
    s.show_id,
    s.title,
    s.description_markdown,
    array_to_json(ARRAY(SELECT (CASE WHEN prod.person_id IS NULL
                      THEN ('org'::person_or_org_label, prod.org_id, o.name)::person_or_org
                      ELSE ('person'::person_or_org_label, prod.person_id, p.name)::person_or_org
                 END)
             FROM producers prod
             LEFT OUTER JOIN people p USING (person_id)
             LEFT OUTER JOIN organizations o USING (org_id)
             WHERE prod.show_id = s.show_id ORDER BY prod.listed_order ASC)) AS producers,
     array_to_json(ARRAY(SELECT sd.show_date from show_dates sd WHERE sd.show_id = s.show_id ORDER BY sd.show_date ASC)) AS dates,
     (
        SELECT to_json(array_agg(s_works))
        FROM (SELECT
          w.work_id, 
          w.title, 
          (SELECT to_json(array_agg(s_authors))
              FROM 
              (SELECT (CASE WHEN a.person_id IS NULL 
                            THEN ('org'::person_or_org_label, a.org_id, o.name)::person_or_org
                            ELSE ('person'::person_or_org_label, a.person_id, p.name)::person_or_org
                       END)
                FROM authorship a
                LEFT OUTER JOIN people p USING (person_id)
                LEFT OUTER JOIN organizations o USING (org_id)
                WHERE a.work_id = w.work_id) AS s_authors) AS authors
          FROM works AS w
          INNER JOIN performances perf USING (work_id)
          WHERE perf.show_id = s.show_id ORDER BY perf.performance_order ASC) AS s_works
     ) AS show_works,
     (
        SELECT show_date FROM show_dates sd WHERE sd.show_id = s.show_id ORDER BY show_date ASC LIMIT 1
     ) AS opening_night
FROM
    shows AS s
ORDER BY opening_night DESC",
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
       get_show_statement=GetShowStatement
     }.


