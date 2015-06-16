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
-module(ghostlight_db_person).
-export([
         get_statement/1,
         db_to_record/2,
         get_inserts/2,
         get_update_commands/2,
         listings_statement/1,
         db_listings_to_record_list/1,
         prepare_statements/2
        ]).

-include("apps/ghostlight/include/ghostlight_data.hrl").


get_statement(#db_state{get_person_statement=GP}) ->
    GP.

listings_statement(#db_state{get_person_listings=GPL}) ->
    GPL.

db_to_record(
    [{ok, []},
     {ok, [{
        PersonId,
        Name,
        DescriptionSrc,
        DescriptionMarkdown,
        Authorships,
        Director,
        Onstage,
        Offstage,
        Links,
        Employee,
        Member,
        Producer
       }]},
     {ok, []}],
     Form) ->

    Description = case Form of html -> DescriptionMarkdown; markdown -> DescriptionSrc end,

    AuthorList = [#work{
                     id = proplists:get_value(<<"work_id">>, Work),
                     title = proplists:get_value(<<"name">>, Work)
                  } || {Work} <- ghostlight_db_utils:decode_not_null(Authorships)],

    OnstageList = [#show{
                      id = proplists:get_value(<<"show_id">>, Show),
                      title = proplists:get_value(<<"title">>, Show),
                      performances = [#performance{
                                        work = #work {
                                                  id = proplists:get_value(<<"work_id">>, RoleObj),
                                                  title = proplists:get_value(<<"title">>, RoleObj)
                                               },
                                        onstage = [#onstage {
                                                     role = proplists:get_value(<<"role">>, RoleObj)
                                                  }]
                                     } || {RoleObj} <- proplists:get_value(<<"roles">>, Show)],
                      producers=[ ghostlight_db_utils:parse_person_or_org(Prod) || Prod <- proplists:get_value(<<"producers">>, Show)],
                      dates=[ iso8601:parse(proplists:get_value(<<"opening_night">>, Show)) ]
                   } || {Show} <- ghostlight_db_utils:decode_not_null(Onstage)],

    OffstageList = [#show{
                       id = proplists:get_value(<<"show_id">>, Show),
                       title = proplists:get_value(<<"title">>, Show),
                       performances = [#performance{
                                         work = #work {
                                                   id = proplists:get_value(<<"work_id">>, RoleObj),
                                                   title = proplists:get_value(<<"title">>, RoleObj)
                                                },
                                         offstage = [#offstage {
                                                      job = proplists:get_value(<<"job">>, RoleObj)
                                                     }]
                                      } || {RoleObj} <- proplists:get_value(<<"jobs">>, Show)],
                       producers=[ ghostlight_db_utils:parse_person_or_org(Prod) || Prod <- proplists:get_value(<<"producers">>, Show)],
                       dates=[ iso8601:parse(proplists:get_value(<<"opening_night">>, Show)) ]
                    } || {Show} <- ghostlight_db_utils:decode_not_null(Offstage)],

    DirectorList = [#show{
                       id = proplists:get_value(<<"show_id">>, Show),
                       title = proplists:get_value(<<"title">>, Show),
                       performances = [#performance{
                                         work = #work {
                                                   id = proplists:get_value(<<"work_id">>, RoleObj),
                                                   title = proplists:get_value(<<"name">>, RoleObj)
                                                }
                                      } || {RoleObj} <- proplists:get_value(<<"works">>, Show)],
                       producers=[ ghostlight_db_utils:parse_person_or_org(Prod) || Prod <- proplists:get_value(<<"producers">>, Show)],
                       dates=[ iso8601:parse(proplists:get_value(<<"opening_night">>, Show)) ]
                    } || {Show} <- ghostlight_db_utils:decode_not_null(Director)],

    EmployeeList = [ #org_work {
                        org_id = proplists:get_value(<<"org_id">>, Emp),
                        org_name = proplists:get_value(<<"name">>, Emp),
                        title = proplists:get_value(<<"title">>, Emp)
                     } || {Emp} <- ghostlight_db_utils:decode_not_null(Employee)],
    MemberList = [ #organization{
                      id = proplists:get_value(<<"org_id">>, Mem),
                      name = proplists:get_value(<<"name">>, Mem)
                   } || {Mem} <- ghostlight_db_utils:decode_not_null(Member)],

    ProducerList = [#show{
                       id = proplists:get_value(<<"show_id">>, Show),
                       title = proplists:get_value(<<"title">>, Show),
                       dates=[ iso8601:parse(proplists:get_value(<<"opening_night">>, Show)) ],
                       performances = [#performance{
                                         work = #work {
                                                   id = proplists:get_value(<<"work_id">>, RoleObj),
                                                   title = proplists:get_value(<<"name">>, RoleObj)
                                                }
                                      } || {RoleObj} <- proplists:get_value(<<"works">>, Show)]
                    } || {Show} <- ghostlight_db_utils:decode_not_null(Producer)],

    ExternalLinks = ghostlight_db_utils:external_links_sql_to_record(ghostlight_db_utils:decode_not_null(Links)),

    #person_return{
       person=#person{
           id = PersonId,
           name = Name,
           description=Description,
           external_links=ExternalLinks
       },
       authored = AuthorList,
       directed = DirectorList,
       onstage = OnstageList,
       offstage = OffstageList,
       orgs_employee = EmployeeList,
       orgs_member = MemberList,
       shows_produced = ProducerList
    }.
 

db_listings_to_record_list(Results) ->
    [ #person{id=PersonId, name=PersonName} || {PersonId, PersonName} <- Results ].


get_inserts(#person{ name=Name,
                     description=Description,
                     external_links=Links }, #db_state{insert_person_statement=IP, insert_person_links_statement=PL}) ->
    PersonId = ghostlight_db_utils:fresh_uuid(),
    Markdowned = ghostlight_db_utils:markdown_or_null(Description),
    LinkInserts = ghostlight_db_utils:external_links_inserts(PersonId, PL, Links),
    AllInserts = lists:append([ [{IP, [PersonId, Name, Description, Markdowned]}],
                                 LinkInserts]),
    {AllInserts, PersonId}.


get_update_commands(#person{id=PersonId,
                            name=Name,
                            description=DescSrc,
                            external_links=Links},
                    #db_state{update_person_statement=UP,
                              delete_person_links_statement=DL,
                              insert_person_links_statement=PL}) ->

    DescMarkdown = ghostlight_db_utils:markdown_or_null(DescSrc),
    LinkInserts = ghostlight_db_utils:external_links_inserts(PersonId, PL, Links),
 
    lists:append([ [{UP, [Name, DescSrc, DescMarkdown, PersonId]},
                    {DL, [PersonId]}],
                    LinkInserts]).


prepare_statements(C, State) ->
    PersonSql = "INSERT INTO people (person_id, name, description_src, description_markdown, photo_id, date_added) VALUES($1, $2, $3, $4, NULL, CURRENT_DATE)", 
    {ok, InsertPerson} = epgsql:parse(C, "insert_person", PersonSql, [uuid, text, text, text]),
    PersonLinksSql = "INSERT INTO people_links (person_id, link, type) VALUES($1, $2, $3::link_type)",
    {ok, PersonLinks} = epgsql:parse(C, "insert_person_links", PersonLinksSql, [uuid, text, text]),

    GetPersonListingsSql = "SELECT p.person_id, p.name FROM people AS p ORDER BY p.name ASC",
    {ok, GetPersonListings} = epgsql:parse(C, "get_person_listings", GetPersonListingsSql, []),

    GetPersonSql =
"
SELECT
    p.person_id,
    p.name,
    p.description_src,
    p.description_markdown,
    array_to_json(ARRAY(SELECT (w.work_id, w.title)::work_pair
                        FROM works w 
                        INNER JOIN authorship a USING (work_id)
                        where a.person_id = p.person_id
                        ORDER BY w.title ASC)) AS authorships,
    -- Director
    (
        SELECT to_json(array_agg(directed))
        FROM (SELECT s.show_id,
                     s.title,
                     array_to_json(ARRAY(SELECT (w.work_id, w.title)::work_pair
                                                FROM works w
                                                INNER JOIN performances perf USING (work_id)
                                                INNER JOIN performance_directors pd USING (performance_id)
                                                WHERE perf.show_id = s.show_id
                                                AND pd.director_id = p.person_id
                                                ORDER BY perf.performance_order)) AS works,
                     array_to_json(ARRAY(SELECT (CASE WHEN prod.person_id IS NULL
                                                     THEN ('org'::person_or_org_label, prod.org_id, o.name)::person_or_org
                                                     ELSE ('person'::person_or_org_label, prod.person_id, p.name)::person_or_org
                                                END)
                                            FROM producers prod
                                            LEFT OUTER JOIN people p USING (person_id)
                                            LEFT OUTER JOIN organizations o USING (org_id)
                                            WHERE prod.show_id = s.show_id ORDER BY prod.listed_order DESC)) AS producers,
                     (
                        SELECT show_date FROM show_dates sd WHERE sd.show_id = s.show_id ORDER BY show_date ASC LIMIT 1
                     ) AS opening_night
               FROM shows s
               INNER JOIN performances perf USING (show_id)
               INNER JOIN performance_directors pd USING (performance_id)
               WHERE pd.director_id = p.person_id
               ORDER BY opening_night DESC) AS directed
    ) AS directorships,
    -- Onstage
    (
        SELECT to_json(array_agg(onstaged))
        FROM (SELECT s.show_id,
                     s.title,
                     (SELECT array_agg(collected)
                      FROM (SELECT w.work_id, w.title, po.role
                                   FROM works w
                                   INNER JOIN performances perf USING (work_id)
                                   WHERE perf.show_id = s.show_id
                                   AND perf.performance_id = po.performance_id
                                   AND po.performer_id = p.person_id
                                   ORDER BY perf.performance_order) AS collected) AS roles,
                     array_to_json(ARRAY(SELECT (CASE WHEN prod.person_id IS NULL
                                                     THEN ('org'::person_or_org_label, prod.org_id, o.name)::person_or_org
                                                     ELSE ('person'::person_or_org_label, prod.person_id, p.name)::person_or_org
                                                END)
                                            FROM producers prod
                                            LEFT OUTER JOIN people p USING (person_id)
                                            LEFT OUTER JOIN organizations o USING (org_id)
                                            WHERE prod.show_id = s.show_id ORDER BY prod.listed_order DESC)) AS producers,
                     (
                        SELECT show_date FROM show_dates sd WHERE sd.show_id = s.show_id ORDER BY show_date ASC LIMIT 1
                     ) AS opening_night
               FROM shows s
               INNER JOIN performances perf USING (show_id)
               INNER JOIN performance_onstage po USING (performance_id)
               WHERE po.performer_id = p.person_id
               ORDER BY opening_night DESC, perf.performance_order ASC) AS onstaged
    ) AS onstage,
    -- Offstage
    (
        SELECT to_json(array_agg(offstaged))
        FROM (SELECT s.show_id,
                     s.title,
                     (SELECT array_agg(collected)
                      FROM (SELECT w.work_id, w.title, po.job
                                   FROM works w
                                   INNER JOIN performances perf USING (work_id)
                                   WHERE perf.show_id = s.show_id
                                   AND perf.performance_id = po.performance_id
                                   AND po.person_id = p.person_id
                                   ORDER BY perf.performance_order) AS collected) AS jobs,
                     array_to_json(ARRAY(SELECT (CASE WHEN prod.person_id IS NULL
                                                     THEN ('org'::person_or_org_label, prod.org_id, o.name)::person_or_org
                                                     ELSE ('person'::person_or_org_label, prod.person_id, p.name)::person_or_org
                                                END)
                                            FROM producers prod
                                            LEFT OUTER JOIN people p USING (person_id)
                                            LEFT OUTER JOIN organizations o USING (org_id)
                                            WHERE prod.show_id = s.show_id ORDER BY prod.listed_order DESC)) AS producers,
                     (
                        SELECT show_date FROM show_dates sd WHERE sd.show_id = s.show_id ORDER BY show_date ASC LIMIT 1
                     ) AS opening_night
               FROM shows s
               INNER JOIN performances perf USING (show_id)
               INNER JOIN performance_offstage po USING (performance_id)
               WHERE po.person_id = p.person_id
               ORDER BY opening_night DESC, perf.performance_order ASC) AS offstaged
    ) AS offstage,
    -- Links
    array_to_json(ARRAY(SELECT (pl.link, pl.type)::external_link
                        FROM people_links pl
                        WHERE pl.person_id = p.person_id)) AS links,
    -- Employee
    (
        SELECT to_json(array_agg(emp))
        FROM (SELECT o.org_id, o.name, oe.title
              FROM organizations o
              INNER JOIN org_employees oe USING (org_id)
              WHERE oe.person_id = p.person_id) AS emp
    ) AS employee,
    -- Member 
    (
        SELECT to_json(array_agg(mem))
        FROM (SELECT o.org_id, o.name
              FROM organizations o
              INNER JOIN org_members om USING (org_id)
              WHERE om.person_id = p.person_id) AS mem
    ) AS member,
    -- Producer
    (
        SELECT to_json(array_agg(prod))
        FROM (SELECT s.show_id,
                     s.title,
                     array_to_json(ARRAY(SELECT (w.work_id, w.title)::work_pair
                                                FROM works w
                                                INNER JOIN performances p USING (work_id)
                                                WHERE p.show_id = s.show_id
                                                ORDER BY p.performance_order ASC)) AS works,
                     (
                        SELECT show_date FROM show_dates sd WHERE sd.show_id = s.show_id ORDER BY show_date ASC LIMIT 1
                     ) AS opening_night
               FROM shows s
               INNER JOIN producers USING (show_id)
               WHERE producers.person_id = p.person_id
               ORDER BY opening_night DESC) AS prod
    ) AS shows_produced
FROM people p
WHERE p.person_id = $1
",
    {ok, GetPerson} = epgsql:parse(C, "get_person_statement", GetPersonSql, [uuid]),


    DeletePersonLinksSql = "DELETE FROM people_links WHERE person_id =  $1",
    {ok, DeletePersonLinks} = epgsql:parse(C, "delete_person_links", DeletePersonLinksSql, [uuid]),

    UpdatePersonSql = "UPDATE people SET name = $1, description_src = $2, description_markdown = $3 WHERE person_id = $4", 
    {ok, UpdatePerson} = epgsql:parse(C, "update_person", UpdatePersonSql, [text, text, text, uuid]),
 
    State#db_state{
       insert_person_statement=InsertPerson,
       insert_person_links_statement=PersonLinks,
       get_person_listings=GetPersonListings,

       delete_person_links_statement=DeletePersonLinks,
       update_person_statement=UpdatePerson,
 
       get_person_statement=GetPerson
    }.


