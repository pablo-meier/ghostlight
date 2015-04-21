%% Orgs are pretty straightforward -- who's in them, and what have they
%% produced. Need to see if I care about in-app membership enough to 
%% make that happen here.
-module(ghostlight_db_org).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([get/1,
         get/2,
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
    lager:error("DB server (ORGS) terminating: ~p", [Reason]),
    epgsql:close(C).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(get_org_listings, _From, State=#db_state{connection=C, get_org_listings=GO}) ->
    epgsql:bind(C, GO, "", []),
    {ok, Rows} = epgsql:execute(C, GO),
    {reply, Rows, State};

handle_call({get_org, OrgId}, _From, State=#db_state{get_org_statement=GO}) ->
    Batch = [ {GO, [OrgId]} ],
    Reply = ghostlight_db_utils:exec_batch(Batch, State),
    {reply, Reply, State};

handle_call({insert_org, Org}, _From, State) ->
    {Inserts, Id} = get_inserts(Org, State),
    Reply = ghostlight_db_utils:exec_batch(Inserts, State),
    lager:info("Postgres returned ~p for insert", [Reply]),
    {reply, Id, State};


handle_call({get_inserts, Org }, _From, State) ->
    Reply = get_inserts(Org, State),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


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

    DescriptionInsert = ghostlight_db_utils:null_if_unspecified(Description),
    Markdowned = ghostlight_db_utils:markdown_or_null(Description),
    
    VanityNameInsert = ghostlight_db_utils:null_if_unspecified(VanityName),
    TaglineInsert = ghostlight_db_utils:null_if_unspecified(Tagline),
    DateFoundedInsert = ghostlight_db_utils:null_if_unspecified(DateFounded),
    MemberInserts = lists:flatten( [ member_inserts(OrgId, Member, State) || Member <- Members ]),
    EmployeeInserts = lists:flatten( [ employee_inserts(OrgId, Employee, State) || Employee <- Employees]),
    LinkInserts = ghostlight_db_utils:external_links_inserts(OrgId, OL, Links),

    OrgInserts = lists:append([ [{IO, [OrgId, ParentInsert, Name, 
                                       TaglineInsert, DescriptionInsert, Markdowned, 
                                       VanityNameInsert, DateFoundedInsert, Visibility]}],
                                EmployeeInserts,
                                MemberInserts,
                                LinkInserts
                              ]),
    {OrgInserts, OrgId}.

member_inserts(OrgId, #org_member{member=Person, description=Description}, #db_state{insert_org_member=IM}) ->
    {PersonInsert, PersonId} = ghostlight_db_person:get_inserts(Person),
    Markdowned = ghostlight_db_utils:markdown_or_null(Description),
    [PersonInsert,
     {IM, [OrgId, PersonId, Description, Markdowned, null, null]}].

employee_inserts(OrgId, #org_employee{person=Person, title=Title, description=Description}, #db_state{insert_org_employee=IE}) ->
    {PersonInsert, PersonId} = ghostlight_db_person:get_inserts(Person),
    Markdowned = ghostlight_db_utils:markdown_or_null(Description),
    [PersonInsert,
     {IE, [OrgId, PersonId, Title, Description, Markdowned, null, null]}].
    

%%%===================================================================
%%% Resource callbacks.
%%%===================================================================

get(OrgId) ->
  get(OrgId, html).

%% Format parameter tells us whether we return Markdown or the Src for any
%% fields that apply.
get(OrgId, Format) ->
    Reply = gen_server:call(?MODULE, {get_org, OrgId}),
    [{ok, []},
     {ok,
      [{
        OrgId,
        OrgName,
        OrgTagline,
        OrgDescriptionSrc,
        OrgDescription,
        Links,
        ShowsProduced,
        Employees,
        Members
       }]},
     {ok, []}] = Reply,

    ExternalLinks = ghostlight_db_utils:external_links_sql_to_record(ghostlight_db_utils:decode_not_null(Links)),
    MemberList = [ parse_member(Member) || Member <- ghostlight_db_utils:decode_not_null(Members)],
    EmployeeList = [ parse_employee(Employee) || Employee <- ghostlight_db_utils:decode_not_null(Employees)],
    ShowList = [ parse_show_abbrev(Show) || Show <- ghostlight_db_utils:decode_not_null(ShowsProduced) ],

    Desc = case Format of html -> OrgDescription; markdown -> OrgDescriptionSrc end,

    #org_return{
      org=#organization{
             id=OrgId,
             name=OrgName,
             tagline=OrgTagline,
             description=Desc,
             external_links=ExternalLinks,
             members=MemberList,
             employees=EmployeeList
          },
      shows_produced = ShowList
    }.

parse_member({Member}) ->
    #org_member{
       member = #person {
                   id = proplists:get_value(<<"person_id">>, Member),
                   name = proplists:get_value(<<"name">>, Member)
                },
       description = proplists:get_value(<<"description">>, Member, null)
    }.

parse_employee({Employee}) ->
    #org_employee{
       person = #person {
                   id = proplists:get_value(<<"person_id">>, Employee),
                   name = proplists:get_value(<<"name">>, Employee)
                },
       title = proplists:get_value(<<"title">>, Employee, null),
       description = proplists:get_value(<<"description">>, Employee, null)
    }.

parse_show_abbrev({Show}) ->
    #show{
       id = proplists:get_value(<<"show_id">>, Show),
       title = proplists:get_value(<<"title">>, Show),
       performances=[#performance{
                        work=#work{ id = proplists:get_value(<<"work_id">>, Work),
                                    title = proplists:get_value(<<"name">>, Work)}} 
                     || {Work} <- proplists:get_value(<<"works">>, Show)
                    ]
    }.

listings() ->
    Results = gen_server:call(?MODULE, get_org_listings),
    [ #organization{
         id=OrgId,
         name=OrgName,
         tagline=Tagline,
         description=Description
        } || {OrgId, OrgName, Tagline, Description} <- Results].

insert(Org) ->
    gen_server:call(?MODULE, {insert_org, Org}).
get_inserts(Org) ->
    gen_server:call(?MODULE, {get_inserts, Org}).


prepare_statements(C, State) ->
    OrgsSql = "INSERT INTO organizations "
        ++ "(org_id, parent_org, name, tagline, description_src, description_markdown, vanity_name, date_founded, visibility)"
        ++ " VALUES($1, $2, $3, $4, $5, $6, $7, $8::date, $9)",
    {ok, InsertOrg} = epgsql:parse(C, "insert_organization", OrgsSql, [uuid, uuid, text, text, text, text, date, text]),

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
    o.tagline,
    o.description_src,
    o.description_markdown,
    array_to_json(ARRAY(SELECT (ol.link, ol.type)::external_link
                            FROM org_links ol
                            WHERE ol.org_id = o.org_id)) AS links,
    (
        SELECT to_json(array_agg(prod))
        FROM (SELECT s.show_id,
                     s.title,
                     array_to_json(ARRAY(SELECT (w.work_id, w.title)::work_pair
                                                FROM works w
                                                INNER JOIN performances p USING (work_id)
                                                WHERE p.show_id = s.show_id ORDER BY p.performance_order)) AS works
               FROM shows s
               INNER JOIN producers USING (show_id)
               WHERE producers.org_id = o.org_id) AS prod
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

    GetOrgListingsSql = "SELECT o.org_id, o.name, o.tagline, o.description_markdown FROM organizations AS o ORDER BY o.name ASC LIMIT 50",
    {ok, GetOrgListings} = epgsql:parse(C, "get_org_listings", GetOrgListingsSql, []),

 
    State#db_state{
       insert_org_statement=InsertOrg,

       get_org_statement=GetOrg,
       get_org_listings=GetOrgListings,

       insert_org_employee=InsertOrgEmployee,
       insert_org_member=InsertOrgMember,
       insert_org_external_link=OrgExternalLink
    }.

