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

handle_call({get_org, OrgId}, _From, State=#db_state{get_org_meta=GOI,
                                                     get_org_show_dates=GOD,
                                                     get_produced_by_org=GPO,
                                                     get_org_employees=GOE,
                                                     get_org_members=GOM,
                                                     get_org_links=GOL}) ->
    Batch = [ {GOI, [OrgId]},
              {GOD, [OrgId]},
              {GPO, [OrgId]},
              {GOE, [OrgId]},
              {GOM, [OrgId]},
              {GOL, [OrgId]} ],
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
           }, State=#db_state{insert_org_statement=IO}) ->
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
    LinkInserts = external_links_inserts(OrgId, Links, State),

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
    
external_links_inserts(OrgId,
                       #external_links{ 
                          website=Website, 
                          email_address=Email, 
                          blog=Blog, 
                          mailing_list=MailingList,
                          facebook=Facebook, 
                          twitter=Twitter, 
                          instagram=Instagram,
                          vimeo=Vimeo,
                          youtube=YouTube
                         }, #db_state{insert_org_external_link=IL}) ->
    WebsiteI = null_or_link_insert(OrgId, Website, <<"website">>, IL),
    EmailI = null_or_link_insert(OrgId, Email, <<"email">>, IL),
    BlogI = null_or_link_insert(OrgId, Blog, <<"blog">>, IL),
    MailingListI = null_or_link_insert(OrgId, MailingList, <<"newsletter">>, IL),
    FacebookI = null_or_link_insert(OrgId, Facebook, <<"facebook">>, IL),
    TwitterI = null_or_link_insert(OrgId, Twitter, <<"twitter">>, IL),
    InstagramI = null_or_link_insert(OrgId, Instagram, <<"instagram">>, IL),
    VimeoI = null_or_link_insert(OrgId, Vimeo, <<"vimeo">>, IL),
    YouTubeI = null_or_link_insert(OrgId, YouTube, <<"youtube">>, IL),
    lists:filter(fun(X) -> X =/= null end, [WebsiteI,
                                            EmailI,
                                            BlogI,
                                            MailingListI,
                                            FacebookI,
                                            TwitterI,
                                            InstagramI,
                                            VimeoI,
                                            YouTubeI]).

null_or_link_insert(OrgId, Link, Type, Stmt) ->
    case Link of
        null -> null;
        _ -> {Stmt, [OrgId, Link, Type]}
    end.


%%%===================================================================
%%% Resource callbacks.
%%%===================================================================
get(OrgId) ->
    Reply = gen_server:call(?MODULE, {get_org, OrgId}),
    [{ok, []},
     {ok, Meta},
     {ok, Dates},
     {ok, Produced},
     {ok, Employees},
     {ok, Members},
     {ok, Links},
     {ok, []}] = Reply,

    [{Name, Tagline, OrgDescription}] = Meta,
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
                        },
                        description=EmpDescription
                     } || {PersonId, PersonName, PersonTitle, EmpDescription} <- Employees ],
    MemberList =[ #org_member{
                        member=#person{
                            id=PersonId,
                            name=PersonName
                        },
                        description=MemDescription
                     } || {PersonId, PersonName, MemDescription} <- Members ],
    ExternalLinks = ghostlight_db_utils:external_links_sql_to_record(Links),
    #org_return{
      org=#organization{
             id=OrgId,
             name=Name,
             tagline=Tagline,
             members=MemberList,
             employees=EmployeeList,
             description=OrgDescription,
             external_links=ExternalLinks
          },
      shows_produced=ShowList
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

    GetOrgMetaSql = "SELECT o.name, o.tagline, o.description_markdown FROM organizations AS o WHERE o.org_id = $1",
    {ok, GetOrgMeta} = epgsql:parse(C, "get_org_meta", GetOrgMetaSql, [uuid]),

    GetProducedByOrgSql = "SELECT s.show_id, s.title, w.work_id, w.title FROM shows AS s "
        ++ "INNER JOIN performances AS p USING (show_id) INNER JOIN organizations AS o ON (o.org_id = s.producing_org_id) "
        ++ "INNER JOIN works AS w ON (p.work_id = w.work_id) WHERE o.org_id = $1",
    {ok, GetProducedByOrg} = epgsql:parse(C, "get_produced_by_org", GetProducedByOrgSql, [uuid]),

    GetDatesOfShowSql = "SELECT s.show_id, s.title, d.show_date FROM shows AS s INNER JOIN "
        ++ "organizations AS o ON (o.org_id = s.producing_org_id) INNER JOIN show_dates AS d "
        ++ "USING (show_id) WHERE o.org_id = $1 ORDER BY d.show_date ASC",
    {ok, GetDatesOfShow} = epgsql:parse(C, "get_dates_of_shows_by_org", GetDatesOfShowSql, [uuid]),

    GetOrgEmployeesSql = "SELECT p.person_id, p.name, oe.title, oe.description_markdown FROM org_employees AS oe INNER JOIN people AS p USING "
        ++ "(person_id) WHERE oe.org_id = $1",
    {ok, GetOrgEmployees} = epgsql:parse(C, "get_org_employees", GetOrgEmployeesSql, [uuid]),
    
    GetOrgMembersSql = "SELECT p.person_id, p.name, om.description_markdown FROM org_members AS om INNER JOIN people AS p USING "
        ++ "(person_id) WHERE om.org_id = $1",
    {ok, GetOrgMembers} = epgsql:parse(C, "get_org_members", GetOrgMembersSql, [uuid]),

    GetOrgLinksSql = "SELECT link, type FROM org_links WHERE org_id = $1",
    {ok, GetOrgLinks} = epgsql:parse(C, "get_org_links", GetOrgLinksSql, [uuid]),

    GetOrgListingsSql = "SELECT o.org_id, o.name, o.tagline, o.description_markdown FROM organizations AS o ORDER BY o.name ASC LIMIT 50",
    {ok, GetOrgListings} = epgsql:parse(C, "get_org_listings", GetOrgListingsSql, []),

 
    State#db_state{
       insert_org_statement=InsertOrg,
       get_org_listings=GetOrgListings,
       get_org_meta=GetOrgMeta,
       get_produced_by_org=GetProducedByOrg,
       get_org_show_dates=GetDatesOfShow,
       get_org_employees=GetOrgEmployees,
       get_org_members=GetOrgMembers,
       get_org_links=GetOrgLinks,

       insert_org_employee=InsertOrgEmployee,
       insert_org_member=InsertOrgMember,
       insert_org_external_link=OrgExternalLink
    }.

