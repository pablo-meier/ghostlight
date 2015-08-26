-module(ghostlight_db_work).

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
-include("apps/ghostlight_db/include/ghostlight_db_statements.hrl").

%%%===================================================================
%%% Resource callbacks.
%%%===================================================================

get_statement(#db_state{get_work_statement=GW}) ->
    GW.

listings_statement(#db_state{get_work_listings=GWL}) ->
    GWL.

db_to_record(
    [{ok, []},
     {ok, [{
        WorkId,
        Title,
        Authors,
        DescriptionSrc,
        DescriptionMarkdown,
        MinutesLong,
        Productions
       }]},
     {ok, []}],
    Format) ->
    Description = case Format of html -> DescriptionMarkdown; markdown -> DescriptionSrc end,

    #work_return{
       work=#work{
               id=WorkId,
               title=Title,
               authors=[ ghostlight_db_utils:parse_person_or_org(Author) || Author <- jsx:decode(Authors) ],
               description=Description,
               minutes_long=MinutesLong
            },
       shows=[ parse_show(Show) || Show <- jsx:decode(Productions) ]
    }.

db_listings_to_record_list(Response) ->
    [ #work{
          id=WorkId,
          title=WorkTitle,
          authors=[ ghostlight_db_utils:parse_person_or_org(Author) || Author <- jsx:decode(Authors) ]
      } || {WorkId, WorkTitle, Authors} <- Response].


get_inserts(#work{title=Title,
                  authors=Authors,
                  description=Description,
                  minutes_long=MinutesLong},
            State=#db_state{insert_work_statement=IW,
                            insert_authorship_statement=IA}) ->
    WorkUUID = ghostlight_db_utils:fresh_uuid(),
    AuthorshipInserts = lists:flatten([ get_author_inserts(WorkUUID, Author, IA, State) || Author <- Authors ]),

    Markdowned = ghostlight_db_utils:markdown_or_null(Description),

    WorkInserts = lists:append([ [{IW, [WorkUUID, Title, Description, Markdowned, MinutesLong, <<"public">>]}],
                                 AuthorshipInserts]),
    {WorkInserts, WorkUUID}.


parse_show(Show) ->
    #show{
       id = proplists:get_value(<<"show_id">>, Show),
       title = proplists:get_value(<<"title">>, Show),
       producers = [ ghostlight_db_utils:parse_person_or_org(Producer) ||
                     Producer <- proplists:get_value(<<"producers">>, Show)]
    }.


get_author_inserts(WorkId, Person=#person{}, Stmt, State) ->
    {PersonInserts, PersonId} = ghostlight_db_person:get_inserts(Person, State),
    lists:append([ PersonInserts,
                   [{Stmt, [WorkId, PersonId, null]}] ]);
get_author_inserts(WorkId, Org=#organization{}, Stmt, State) ->
    {OrgInserts, OrgId} = ghostlight_db_org:get_inserts(Org, State),
    lists:append([ OrgInserts,
                   [{Stmt, [WorkId, null, OrgId]}] ]).


get_update_commands(#work{id=WorkId,
                          title=Title,
                          description=DescSrc,
                          minutes_long=MinutesLong,
                          authors=Authors},
                    State=#db_state{update_work_statement=UW,
                                    delete_authors_statement=DA,
                                    insert_authorship_statement=IA}) ->

    DescMarkdown = ghostlight_db_utils:markdown_or_null(DescSrc),
    AuthorshipInserts = lists:flatten([ get_author_inserts(WorkId, Author, IA, State) || Author <- Authors ]),

    lager:info("WorkId is ~p~n", [WorkId]),

    lists:append([ [{UW, [Title, DescSrc, DescMarkdown, MinutesLong, WorkId]},
                    {DA, [WorkId]}],
                   AuthorshipInserts]).


prepare_statements(C, State) ->
    WorksSql = "INSERT INTO works (work_id, title, description_src, description_markdown, minutes_long, acl) VALUES($1, $2, $3, $4, $5, $6)", 
    {ok, InsertWork} = epgsql:parse(C, "insert_work", WorksSql, [uuid, text, text, text, int8, text]),
    AuthorshipSql = "INSERT INTO authorship (work_id, person_id, org_id) VALUES($1, $2, $3)",
    {ok, InsertAuthorship} = epgsql:parse(C, "insert_authorship", AuthorshipSql, [uuid, uuid, uuid]),

    GetWorkSql =
"
SELECT
    w.work_id,
    w.title,
    array_to_json(ARRAY(SELECT (CASE WHEN a.person_id IS NULL
                                   THEN ('org'::person_or_org_label, a.org_id, o.name)::person_or_org
                                   ELSE ('person'::person_or_org_label, a.person_id, p.name)::person_or_org
                               END)
                        FROM authorship a
                        LEFT OUTER JOIN people p USING (person_id)
                        LEFT OUTER JOIN organizations o USING (org_id)
                        WHERE a.work_id = w.work_id)) AS authors,
    w.description_src,
    w.description_markdown,
    w.minutes_long,
    array_to_json(ARRAY(SELECT (s.show_id,
                                s.title,
                                ARRAY(SELECT (CASE WHEN prod.person_id IS NULL
                                                  THEN ('org'::person_or_org_label, prod.org_id, o.name)::person_or_org
                                                  ELSE ('person'::person_or_org_label, prod.person_id, p.name)::person_or_org
                                              END)
                                       FROM producers prod
                                       LEFT OUTER JOIN people p USING (person_id)
                                       LEFT OUTER JOIN organizations o USING (org_id)
                                       WHERE prod.show_id = s.show_id ORDER BY prod.listed_order DESC))::production_abbrev
                        FROM shows s 
                        INNER JOIN performances p USING (show_id)
                        WHERE p.work_id = w.work_id)) AS productions
FROM works w WHERE work_id = $1;
",
    {ok, GetWork} = epgsql:parse(C, "get_work_statement", GetWorkSql, [uuid]),

    %% For work listings -- much like the meta of a single one.
    GetWorkListingsSql = "
SELECT 
    w.work_id,
    w.title,
    array_to_json(ARRAY(SELECT (CASE WHEN a.person_id IS NULL
                                   THEN ('org'::person_or_org_label, a.org_id, o.name)::person_or_org
                                   ELSE ('person'::person_or_org_label, a.person_id, p.name)::person_or_org
                               END)
                        FROM authorship a
                        LEFT OUTER JOIN people p USING (person_id)
                        LEFT OUTER JOIN organizations o USING (org_id)
                        WHERE a.work_id = w.work_id)) AS authors
FROM works AS w
ORDER BY w.title ASC",
    {ok, GetWorkListings} = epgsql:parse(C, "get_work_listings", GetWorkListingsSql, []),

    UpdateWorkSql = "UPDATE works SET title = $1, description_src = $2, description_markdown = $3, minutes_long = $4 WHERE work_id = $5",
    {ok, UpdateWork} = epgsql:parse(C, "update_work", UpdateWorkSql, [text, text, text, int8, uuid]),
    DeleteAuthorsSql = "DELETE FROM authorship WHERE work_id = $1",
    {ok, DeleteAuthors} = epgsql:parse(C, "delete_authorship", DeleteAuthorsSql, [uuid]),


    State#db_state{
       insert_work_statement=InsertWork,
       insert_authorship_statement=InsertAuthorship,

       get_work_statement=GetWork,

       update_work_statement=UpdateWork,
       delete_authors_statement=DeleteAuthors,

       get_work_listings=GetWorkListings
    }.


