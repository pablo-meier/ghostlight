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
    [{OrgObj, ShowsProducedJson}]},
    {ok, []}],
  _Format) ->
    Org = ghostlight_org:json_to_record(jsx:decode(OrgObj)),
    ShowsProduced = [ ghostlight_show:json_to_record(Show)
                      || Show <- ghostlight_db_utils:decode_not_null(ShowsProducedJson) ],
    #org_return{
       org=Org,
       shows_produced=ShowsProduced
    }.


db_listings_to_record_list(Results) ->
    [ #organization{
         id=OrgId,
         name=OrgName,
         tagline=Tagline,
         description=Description
        } || {OrgId, OrgName, Tagline, Description} <- Results].


get_inserts(#organization {
                name=Name,
                tagline=Tagline,
                description=Description,
                parent=Parent,
                vanity_name=VanityName,
                date_founded=DateFounded,
                members=Members,
                employees=Employees,
                external_links=Links,
                visibility=Visibility
           }, State=#db_state{insert_org_statement=IO,
                              insert_org_external_link=OL}) ->
    OrgId = ghostlight_db_utils:fresh_uuid(),
    ParentInsert = case Parent of
                       {id, <<"">>} -> null;
                       {id, Valid} -> Valid
                   end,

    TaglineInsert = ghostlight_db_utils:null_if_unspecified(Tagline),
    TaglineMarkdown = ghostlight_db_utils:markdown_or_null(Tagline),

    DescriptionInsert = ghostlight_db_utils:null_if_unspecified(Description),
    Markdowned = ghostlight_db_utils:markdown_or_null(Description),
    
    VanityNameInsert = ghostlight_db_utils:null_if_unspecified(VanityName),
    DateFoundedInsert = ghostlight_db_utils:null_if_unspecified(DateFounded),
    MemberInserts = lists:flatten( [ member_inserts(OrgId, Member, State) || Member <- Members ]),
    EmployeeInserts = lists:flatten( [ employee_inserts(OrgId, Employee, State) || Employee <- Employees]),
    LinkInserts = ghostlight_db_utils:external_links_inserts(OrgId, OL, Links),

    OrgInserts = lists:append([ [{IO, [OrgId, ParentInsert, Name, 
                                       TaglineInsert, TaglineMarkdown, DescriptionInsert, Markdowned, 
                                       VanityNameInsert, DateFoundedInsert, Visibility]}],
                                EmployeeInserts,
                                MemberInserts,
                                LinkInserts
                              ]),
    {OrgInserts, OrgId}.


member_inserts(OrgId, #org_member{member=Person, description=Description}, State=#db_state{insert_org_member=IM}) ->
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
        ++ "(org_id, parent_org, name, tagline_src, tagline_markdown, description_src, description_markdown, vanity_name, date_founded, visibility)"
        ++ " VALUES($1, $2, $3, $4, $5, $6, $7, $8, $9::date, $10)",
    {ok, InsertOrg} = epgsql:parse(C, "insert_organization", OrgsSql, [uuid, uuid, text, text, text, text, text, date, text]),

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
    po.json AS org,
    ops.shows AS shows_produced
FROM
    parseable_org AS po
    INNER JOIN org_produced_shows AS ops USING (org_id)
WHERE po.org_id = $1
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


