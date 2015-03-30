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
         get_inserts/1]).

-define(SERVER, ?MODULE).

-include("apps/ghostlight/include/ghostlight_data.hrl").

-record(state, {connection,
                begin_statement,
                commit_statement,

                insert_person_statement,
                get_person_listings,
                get_person_name,
                get_person_authorship,
                get_person_orgs,
                get_person_onstage,
                get_person_offstage,
                get_person_directorships
               }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
init([]) ->
    C = ghostlight_db_utils:connect_to_postgres(),
    State = prepare_statements(C),
    {ok, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(Reason, #state{connection=C}) ->
    lager:error("DB server (PEOPLE) terminating: ~p", [Reason]),
    epgsql:close(C).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(get_person_listings, _From, State=#state{connection=C, get_person_listings=GP}) ->
    epgsql:bind(C, GP, "", []),
    {ok, Rows} = epgsql:execute(C, GP),
    {reply, Rows, State};


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


handle_call({insert_person, Person}, _From, State) ->
    {Inserts, _Ids} = get_inserts(Person),
    Reply = exec_batch(Inserts, State),
    {reply, Reply, State};

handle_call({get_inserts, #person{
                            id=_Id,
                            name=Name}},
            _From, 
            State=#state{insert_person_statement=IP}) ->
    PersonId = ghostlight_db_utils:fresh_uuid(),
    Reply = {{IP, [PersonId, Name]}, PersonId},
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.



%%%===================================================================
%%% Resource callbacks.
%%%===================================================================

get(PersonId) ->
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

listings() ->
    Response = gen_server:call(?MODULE, get_person_listings),
    [ #person{id=PersonId, name=PersonName} || {PersonId, PersonName} <- Response ].

insert(Person) ->
    gen_server:call(?MODULE, {insert_person, Person}).

get_inserts(Person) ->
    gen_server:call(?MODULE, {get_inserts, Person}).


prepare_statements(C) ->
    {ok, BeginStmt} = epgsql:parse(C, "begin_statement", "BEGIN", []),
    {ok, CommitStmt} = epgsql:parse(C, "commit_statement", "COMMIT", []),

    PersonSql = "INSERT INTO people (person_id, name, photo_url, date_added) VALUES($1, $2, NULL, CURRENT_DATE)", 
    {ok, InsertPerson} = epgsql:parse(C, "insert_person", PersonSql, [uuid, text]),

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


    #state{
       connection=C,
       begin_statement=BeginStmt,
       commit_statement=CommitStmt,
       insert_person_statement=InsertPerson,

       get_person_listings=GetPersonListings,
       get_person_name=GetPersonName,
       get_person_authorship=GetShowsAuthored,
       get_person_orgs=GetPersonOrgs,
       get_person_onstage=GetPersonOnstage,
       get_person_offstage=GetPersonOffstage,
       get_person_directorships=GetPersonDirected
    }.


%% TODO Put this in one place? The #state bit makes it hard tho.
exec_batch(Batch, #state{connection=C,
                         commit_statement=COMMIT,
                         begin_statement=BEGIN}) ->
    AsTransaction = lists:append([ [{BEGIN, []}],
                                   Batch,
                                   [{COMMIT, []}] ]),
    Results = epgsql:execute_batch(C, AsTransaction),
    Results.

