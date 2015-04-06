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
    lager:error("DB server (PEOPLE) terminating: ~p", [Reason]),
    epgsql:close(C).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(get_person_listings, _From, State=#db_state{connection=C, get_person_listings=GP}) ->
    epgsql:bind(C, GP, "", []),
    {ok, Rows} = epgsql:execute(C, GP),
    {reply, Rows, State};


handle_call({get_person, PersonId}, _From, State=#db_state{get_person_name=GN,
                                                           get_person_authorship=GA,
                                                           get_person_orgs=GO,
                                                           get_person_memberships=GOM,
                                                           get_person_directorships=GD,
                                                           get_person_onstage=POn,
                                                           get_person_offstage=POff,
                                                           get_person_links=GPL}) ->
    Batch = [ {GN, [PersonId]},
              {GA, [PersonId]},
              {GO, [PersonId]},
              {GOM, [PersonId]},
              {GD, [PersonId]},
              {POn, [PersonId]},
              {POff, [PersonId]},
              {GPL, [PersonId]} ],
    Reply = ghostlight_db_utils:exec_batch(Batch, State),
    {reply, Reply, State};


handle_call({insert_person, Person}, _From, State) ->
    {Inserts, Id} = get_inserts(Person, State),
    Reply = ghostlight_db_utils:exec_batch(Inserts, State),
    lager:info("Postgres responsed ~p for person insert", [Reply]),
    {reply, Id, State};

handle_call({get_inserts, Person}, _From, State) ->
    Reply = get_inserts(Person, State),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


get_inserts(#person{ name=Name,
                     description=Description,
                     external_links=Links }, #db_state{insert_person_statement=IP, insert_person_links_statement=PL}) ->
    PersonId = ghostlight_db_utils:fresh_uuid(),
    Markdowned = ghostlight_db_utils:markdown_or_null(Description),
    LinkInserts = ghostlight_db_utils:external_links_inserts(PersonId, PL, Links),
    AllInserts = lists:append([ [{IP, [PersonId, Name, Description, Markdowned]}],
                                 LinkInserts]),
    {AllInserts, PersonId}.


%%%===================================================================
%%% Resource callbacks.
%%%===================================================================

get(PersonId) ->
    Response = gen_server:call(?MODULE, {get_person, PersonId}),
    [{ok, _},
     {ok, [{Name, Description}]},
     {ok, AuthorshipList},
     {ok, OrgsList},
     {ok, Memberships},
     {ok, DirectedList},
     {ok, OnstageList},
     {ok, OffstageList},
     {ok, Links},
     {ok, _}] = Response,
    Authors = [ #work{id=WorkId, title=Title } || {WorkId, Title, _Author} <- AuthorshipList],
    OrgsAsEmployee = [ #org_work{org_id=OrgId, org_name=OrgName, title=Job} || {OrgId, OrgName, Job} <- OrgsList],
    OrgsAsMember = [ #organization{id=OrgId, name=OrgName, description=OrgDescription} || {OrgId, OrgName, OrgDescription} <- Memberships],
    Onstage = [ #show{ title=ShowTitle,
                       id=ShowId,
%                       org=#organization{id=OrgId, name=OrgName},
                       performances=[#performance{
                                       work=#work{ id=WorkId, title=WorkTitle },
                                       onstage=#onstage{ role=Role }
                                    }]
                     } || {ShowId, ShowTitle, WorkId, WorkTitle, _OrgId, _OrgName, Role} <- OnstageList ],
    Offstage = [ #show{ title=ShowTitle,
                       id=ShowId,
%                       org=#organization{id=OrgId, name=OrgName},
                       performances=[#performance{
                                       work=#work{ id=WorkId, title=WorkTitle },
                                       offstage=#offstage{ job=Job }
                                    }]
                     } || {ShowId, ShowTitle, WorkId, WorkTitle, _OrgId, _OrgName, Job} <- OffstageList ],
    Directed = [ #show{ title=ShowTitle,
                        id=ShowId,
%                        org=#organization{id=OrgId, name=OrgName},
                        performances=[#performance{
                                        work=#work{ id=WorkId, title=WorkTitle }
                                     }]
                      } || {ShowId, ShowTitle, WorkId, WorkTitle, _OrgId, _OrgName} <- DirectedList ],
    ExternalLinks = ghostlight_db_utils:external_links_sql_to_record(Links),
 
    #person_return{
       person=#person{
           id = PersonId,
           name = Name,
           description=Description,
           external_links=ExternalLinks
       },
       authored = Authors,
       directed = Directed,
       onstage = Onstage,
       offstage = Offstage,
       orgs_employee = OrgsAsEmployee,
       orgs_member = OrgsAsMember
    }.

listings() ->
    Response = gen_server:call(?MODULE, get_person_listings),
    [ #person{id=PersonId, name=PersonName} || {PersonId, PersonName} <- Response ].

insert(Person) ->
    gen_server:call(?MODULE, {insert_person, Person}).

get_inserts(Person) ->
    gen_server:call(?MODULE, {get_inserts, Person}).


prepare_statements(C, State) ->
    PersonSql = "INSERT INTO people (person_id, name, description_src, description_markdown, photo_id, date_added) VALUES($1, $2, $3, $4, NULL, CURRENT_DATE)", 
    {ok, InsertPerson} = epgsql:parse(C, "insert_person", PersonSql, [uuid, text, text, text]),

    PersonLinksSql = "INSERT INTO people_links (person_id, link, type) VALUES($1, $2, $3::link_type)",
    {ok, PersonLinks} = epgsql:parse(C, "insert_person_links", PersonLinksSql, [uuid, text, text]),

    GetPersonNameSql = "SELECT name, description_markdown FROM people WHERE person_id = $1",
    {ok, GetPersonName} = epgsql:parse(C, "get_person_name", GetPersonNameSql, [uuid]),
    
    %% Pulls the works authored by a person.
    GetShowsAuthoredSql = "SELECT w.work_id, w.title, p.name FROM authorship AS a "
        ++ "INNER JOIN works AS w USING (work_id) INNER JOIN people AS p using (person_id) "
        ++ "WHERE w.acl = 'public' AND p.person_id = $1",
    {ok, GetShowsAuthored} = epgsql:parse(C, "get_authorships", GetShowsAuthoredSql, [uuid]),

    GetPersonOrgsSql = "SELECT o.org_id, o.name, oe.title FROM organizations AS o INNER JOIN org_employees AS oe "
        ++ "USING (org_id) WHERE oe.person_id = $1 AND o.visibility = 'public'",
    {ok, GetPersonOrgs} = epgsql:parse(C, "get_person_orgs", GetPersonOrgsSql, [uuid]),

    GetOrgMembershipsSql = "SELECT o.org_id, o.name, o.description_markdown FROM organizations AS o INNER JOIN "
        ++ "org_members AS om USING (org_id) WHERE om.person_id = $1 AND o.visibility = 'public'",
    {ok, GetOrgMemberships} = epgsql:parse(C, "get_person_org_memberships", GetOrgMembershipsSql, [uuid]),

    GetPersonOnstageSql = "SELECT s.show_id, s.title, w.work_id, w.title, o.org_id, o.name, po.role FROM shows AS s "
        ++ "INNER JOIN performances AS p USING (show_id) INNER JOIN producers AS prod USING (show_id) "
        ++ "INNER JOIN organizations AS o USING (org_id) "
        ++ "INNER JOIN performance_onstage AS po USING (performance_id) INNER JOIN works AS w ON (p.work_id = w.work_id) "
        ++ "WHERE po.performer_id = $1",
    {ok, GetPersonOnstage} = epgsql:parse(C, "get_person_onstage", GetPersonOnstageSql, [uuid]),

    GetPersonOffstageSql = "SELECT s.show_id, s.title, w.work_id, w.title, o.org_id, o.name, po.job FROM shows AS s "
        ++ "INNER JOIN performances AS p USING (show_id) INNER JOIN producers AS prod USING (show_id) "
        ++ "INNER JOIN organizations AS o USING (org_id) "
        ++ "INNER JOIN performance_offstage AS po USING (performance_id) INNER JOIN works AS w ON (p.work_id = w.work_id) "
        ++ "WHERE po.person_id = $1",
    {ok, GetPersonOffstage} = epgsql:parse(C, "get_person_offstage", GetPersonOffstageSql, [uuid]),

    GetPersonDirectedSql = "SELECT s.show_id, s.title, w.work_id, w.title, o.org_id, o.name FROM shows AS s "
        ++ "INNER JOIN performances AS p USING (show_id) INNER JOIN producers AS prod USING (show_id) "
        ++ "INNER JOIN organizations AS o USING (org_id) "
        ++ "INNER JOIN performance_directors AS pd USING (performance_id) INNER JOIN works AS w ON (p.work_id = w.work_id) "
        ++ "WHERE pd.director_id = $1",
    {ok, GetPersonDirected} = epgsql:parse(C, "get_person_directed", GetPersonDirectedSql, [uuid]),

    GetPersonLinksSql = "SELECT link, type FROM people_links WHERE person_id = $1",
    {ok, GetPersonLinks} = epgsql:parse(C, "get_people_links", GetPersonLinksSql, [uuid]),

    GetPersonListingsSql = "SELECT p.person_id, p.name FROM people AS p ORDER BY p.name ASC",
    {ok, GetPersonListings} = epgsql:parse(C, "get_person_listings", GetPersonListingsSql, []),

    State#db_state{
       insert_person_statement=InsertPerson,
       insert_person_links_statement=PersonLinks,
       get_person_listings=GetPersonListings,
       get_person_name=GetPersonName,
       get_person_authorship=GetShowsAuthored,
       get_person_orgs=GetPersonOrgs,
       get_person_onstage=GetPersonOnstage,
       get_person_offstage=GetPersonOffstage,
       get_person_directorships=GetPersonDirected,
       get_person_links=GetPersonLinks,
       get_person_memberships=GetOrgMemberships
    }.


