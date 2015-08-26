%% Orgs are pretty straightforward -- who's in them, and what have they
%% produced. Need to see if I care about in-app membership enough to 
%% make that happen here.
-module(ghostlight_db_org).

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
%%% API callbacks
%%%===================================================================
get_statement(#db_state{get_org_statement=GO}) ->
    GO.

listings_statement(#db_state{get_org_listings=GOL}) ->
    GOL.


db_to_record(
    [{ok, []},
     {ok,
      [{
        OrgId,
        OrgName,
        OrgTaglineSrc,
        OrgTagline,
        OrgDescriptionSrc,
        OrgDescription,
        Links,
        ShowsProduced,
        Employees,
        Members
       }]},
     {ok, []}],
     Format) ->

    ExternalLinks = ghostlight_db_utils:external_links_sql_to_record(ghostlight_db_utils:decode_not_null(Links)),
    MemberList = [ parse_member(Member, Format) || Member <- ghostlight_db_utils:decode_not_null(Members)],
    EmployeeList = [ parse_employee(Employee, Format) || Employee <- ghostlight_db_utils:decode_not_null(Employees)],
    ShowList = [ parse_show_abbrev(Show) || Show <- ghostlight_db_utils:decode_not_null(ShowsProduced) ],

    Desc = case Format of html -> OrgDescription; markdown -> OrgDescriptionSrc end,
    Tag = case Format of html -> OrgTagline; markdown -> OrgTaglineSrc end,

    #org_return{
      org=#organization{
             id=OrgId,
             name=OrgName,
             tagline=Tag,
             description=Desc,
             external_links=ExternalLinks,
             members=MemberList,
             employees=EmployeeList
          },
      shows_produced = ShowList
    }.


db_listings_to_record_list(Results) ->
    [ #organization{
         id=OrgId,
         name=OrgName,
         tagline=Tagline,
         description=Description
        } || {OrgId, OrgName, Tagline, Description} <- Results].


parse_member(Member, Format) ->
    DescType = case Format of html -> <<"description">>; markdown -> <<"description_src">> end,
    #org_member{
       person = #person {
                   id = proplists:get_value(<<"person_id">>, Member),
                   name = proplists:get_value(<<"name">>, Member)
                },
       description = proplists:get_value(DescType, Member, null)
    }.


parse_employee(Employee, Format) ->
    DescType = case Format of html -> <<"description">>; markdown -> <<"description_src">> end,
    #org_employee{
       person = #person {
                   id = proplists:get_value(<<"person_id">>, Employee),
                   name = proplists:get_value(<<"name">>, Employee)
                },
       title = proplists:get_value(<<"title">>, Employee, null),
       description = proplists:get_value(DescType, Employee, null)
    }.


parse_show_abbrev(Show) ->
    #show{
       id = proplists:get_value(<<"show_id">>, Show),
       title = proplists:get_value(<<"title">>, Show),
       performances = [#performance{
                           work=#work{ id = proplists:get_value(<<"work_id">>, Work),
                                       title = proplists:get_value(<<"name">>, Work)}}
                       || {Work} <- proplists:get_value(<<"works">>, Show)
                    ],
       dates = [ iso8601:parse(Date) || Date <- proplists:get_value(<<"show_dates">>, Show) ]
    }.


get_inserts(#organization {
                name=Name,
                tagline=Tagline,
                description=Description,
                vanity_name=VanityName,
                date_founded=DateFounded,
                members=Members,
                employees=Employees,
                external_links=Links
           }, State=#db_state{insert_org_statement=IO,
                              insert_org_external_link=OL}) ->
    OrgId = ghostlight_db_utils:fresh_uuid(),

    TaglineInsert = ghostlight_db_utils:null_if_unspecified(Tagline),
    TaglineMarkdown = ghostlight_db_utils:markdown_or_null(Tagline),

    DescriptionInsert = ghostlight_db_utils:null_if_unspecified(Description),
    Markdowned = ghostlight_db_utils:markdown_or_null(Description),
    
    VanityNameInsert = ghostlight_db_utils:null_if_unspecified(VanityName),
    DateFoundedInsert = ghostlight_db_utils:null_if_unspecified(DateFounded),
    MemberInserts = lists:flatten( [ member_inserts(OrgId, Member, State) || Member <- Members ]),
    EmployeeInserts = lists:flatten( [ employee_inserts(OrgId, Employee, State) || Employee <- Employees]),
    LinkInserts = ghostlight_db_utils:external_links_inserts(OrgId, OL, Links),

    OrgInserts = lists:append([ [{IO, [OrgId, Name, 
                                       TaglineInsert, TaglineMarkdown, DescriptionInsert, Markdowned, 
                                       VanityNameInsert, DateFoundedInsert]}],
                                EmployeeInserts,
                                MemberInserts,
                                LinkInserts
                              ]),
    {OrgInserts, OrgId}.


member_inserts(OrgId, #org_member{person=Person, description=Description}, State=#db_state{insert_org_member=IM}) ->
    {PersonInsert, PersonId} = ghostlight_db_person:get_inserts(Person, State),
    Markdowned = ghostlight_db_utils:markdown_or_null(Description),
    [PersonInsert,
     {IM, [OrgId, PersonId, Description, Markdowned, null, null]}].

employee_inserts(OrgId, #org_employee{person=Person, title=Title, description=Description}, State=#db_state{insert_org_employee=IE}) ->
    {PersonInsert, PersonId} = ghostlight_db_person:get_inserts(Person, State),
    Markdowned = ghostlight_db_utils:markdown_or_null(Description),
    [PersonInsert,
     {IE, [OrgId, PersonId, Title, Description, Markdowned, null, null]}].


%% Updates are "as they stand" - Delete everything that's there, replace
%% it with what you were given.
get_update_commands(#organization{id=OrgId,
                                  name=Name,
                                  tagline=TaglineSrc,
                                  description=DescSrc,
                                  members=Members,
                                  employees=Employees,
                                  external_links=Links},
                    State=#db_state{update_org_statement=UO,
                                    delete_org_employees=DOW,
                                    delete_org_members=DOM,
                                    delete_org_links=DOL,
                                    insert_org_external_link=OL}) ->
    DescMarkdown = ghostlight_db_utils:markdown_or_null(DescSrc),
    TaglineMarkdown = ghostlight_db_utils:markdown_or_null(TaglineSrc),

    MemberInserts = lists:flatten( [ member_inserts(OrgId, Member, State) || Member <- Members ]),
    EmployeeInserts = lists:flatten( [ employee_inserts(OrgId, Employee, State) || Employee <- Employees]),
    LinkInserts = ghostlight_db_utils:external_links_inserts(OrgId, OL, Links),
    lists:append([ [{UO, [Name, TaglineSrc, TaglineMarkdown, DescSrc, DescMarkdown, OrgId]},
                    {DOW, [OrgId]},
                    {DOM, [OrgId]},
                    {DOL, [OrgId]}],
                    EmployeeInserts,
                    MemberInserts,
                    LinkInserts ]).


prepare_statements(C, State) ->
    OrgsSql = "INSERT INTO organizations "
        ++ "(org_id, name, tagline_src, tagline_markdown, description_src, description_markdown, vanity_name, date_founded)"
        ++ " VALUES($1, $2, $3, $4, $5, $6, $7, $8::date)",
    {ok, InsertOrg} = epgsql:parse(C, "insert_organization", OrgsSql, [uuid, uuid, text, text, text, text, text, date]),

    OrgEmployeeSql = "INSERT INTO org_employees "
        ++ "(org_id, person_id, title, description_src, description_markdown, date_started, date_ended)"
        ++ " VALUES($1, $2, $3, $4, $5, $6::date, $7::date)",
    {ok, InsertOrgEmployee} = epgsql:parse(C, "insert_org_employee", OrgEmployeeSql, [uuid, uuid, text, text, text, date, date]),

    OrgMemberSql = "INSERT INTO org_members"
        ++ "(org_id, person_id, description_src, description_markdown, date_started, date_ended)"
        ++ " VALUES($1, $2, $3, $4, $5::date, $6::date)",
    {ok, InsertOrgMember} = epgsql:parse(C, "insert_org_member", OrgMemberSql, [uuid, uuid, text, text, date, date]),

    OrgExternalLinkSql = "INSERT INTO org_links (org_id, link, type) VALUES ($1, $2, $3::link_type)",
    {ok, OrgExternalLink} = epgsql:parse(C, "insert_org_external", OrgExternalLinkSql, [uuid, text, text]),

    GetOrgSql =
"
SELECT
    o.org_id,
    o.name,
    o.tagline_src,
    o.tagline_markdown,
    o.description_src,
    o.description_markdown,
    array_to_json(ARRAY(SELECT (ol.link, ol.type)::external_link
                            FROM org_links ol
                            WHERE ol.org_id = o.org_id)) AS links,
    (
        SELECT to_json(array_agg(prod))
        FROM (SELECT s.show_id,
                     s.title,
                     array_to_json(ARRAY(SELECT (w.work_id, w.title)::titled_pair
                                                FROM works w
                                                INNER JOIN performances p USING (work_id)
                                                WHERE p.show_id = s.show_id ORDER BY p.performance_order)) AS works,
                     array_to_json(ARRAY(SELECT sd.show_date FROM show_dates sd WHERE sd.show_id = s.show_id ORDER BY sd.show_date DESC)) AS show_dates,
                     (
                        SELECT show_date FROM show_dates sd WHERE sd.show_id = s.show_id ORDER BY show_date ASC LIMIT 1
                     ) AS opening_night
               FROM shows s
               INNER JOIN producers USING (show_id)
               WHERE producers.org_id = o.org_id
               ORDER BY opening_night DESC) AS prod
    ) AS shows_produced,
    (
        SELECT to_json(array_agg(emp))
        FROM (SELECT oe.org_id, p.person_id, p.name, oe.title, oe.description_src, oe.description_markdown AS description
              FROM org_employees oe
              INNER JOIN people p USING (person_id)) AS emp
        WHERE emp.org_id = o.org_id
    ) AS employees, 
    (
        SELECT to_json(array_agg(mem))
        FROM (SELECT om.org_id, p.person_id, p.name, om.description_src, om.description_markdown AS description
              FROM org_members om
              INNER JOIN people p USING (person_id)) AS mem
        WHERE mem.org_id = o.org_id
    ) AS members
FROM organizations o
WHERE o.org_id = $1
",
    {ok, GetOrg} = epgsql:parse(C, "get_org_statement", GetOrgSql, [uuid]),

    GetOrgListingsSql = "SELECT o.org_id, o.name, o.tagline_markdown, o.description_markdown FROM organizations AS o ORDER BY o.name ASC LIMIT 50",
    {ok, GetOrgListings} = epgsql:parse(C, "get_org_listings", GetOrgListingsSql, []),

    UpdateOrgSql = "UPDATE organizations "
        ++ "SET name = $1, tagline_src = $2, tagline_markdown = $3, description_src = $4, description_markdown = $5 "
        ++ " WHERE org_id = $6",
    {ok, UpdateOrg} = epgsql:parse(C, "update_organization", UpdateOrgSql, [text, text, text, text, text, uuid]),
  
    DeleteEmployeeSql = "DELETE FROM org_employees WHERE org_id = $1",
    {ok, DeleteOrgEmployee} = epgsql:parse(C, "delete_org_employees", DeleteEmployeeSql, [uuid]),

    DeleteMemberSql = "DELETE FROM org_members WHERE org_id = $1",
    {ok, DeleteOrgMember} = epgsql:parse(C, "delete_org_members", DeleteMemberSql, [uuid]),

    DeleteExternalLinkSql = "DELETE FROM org_links WHERE org_id = $1",
    {ok, DeleteExternalLink} = epgsql:parse(C, "delete_org_external", DeleteExternalLinkSql, [uuid]),

    State#db_state{
       insert_org_statement=InsertOrg,

       get_org_statement=GetOrg,
       get_org_listings=GetOrgListings,

       insert_org_employee=InsertOrgEmployee,
       insert_org_member=InsertOrgMember,
       insert_org_external_link=OrgExternalLink,

       delete_org_employees=DeleteOrgEmployee,
       delete_org_members=DeleteOrgMember,
       delete_org_links=DeleteExternalLink,

       update_org_statement=UpdateOrg
    }.
