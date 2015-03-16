-module(ghostlight_db).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {connection,
                begin_statement,
                commit_statement,
                insert_work_statement,
                insert_authorship_statement,
                insert_person_statement,
                insert_org_statement,
                insert_performance_statement,
                insert_onstage_statement,
                insert_offstage_statement,
                insert_show_statement
                }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, C} = epgsql:connect("localhost", "pablo", "", [{database, "ghostlight-dev"}]),
    initialize_tables(C),
    {ok, BeginStmt} = epgsql:parse(C, "BEGIN"),
    {ok, CommitStmt} = epgsql:parse(C, "COMMIT"),
    {ok, InsertWork} = epgsql:parse(C, "INSERT INTO works (work_id, title, visibility) VALUES($1, $2, $3)", [uuid, text, text]),
    {ok, InsertAuthorship} = epgsql:parse(C, "INSERT INTO authorship (work_id, person_id) VALUES($1, $2)", [uuid, uuid]),
    {ok, InsertPerson} = epgsql:parse(C, "INSERT INTO people (person_id, name, photo_url, date_added) VALUES($1, $2, NULL, CURRENT_DATE)", [uuid, text]),
    {ok, InsertOrg} = epgsql:parse(C, "INSERT INTO organizations (org_id, parent_org, name, tagline, description, vanity_name, date_founded, visibility)"
        ++ " VALUES($1, $2, $3, $4, $5, $6, $7::date, $8)", [uuid, uuid, text, text, text, text, text, date, text]),
    {ok, InsertPerformance} = epgsql:parse(C, "INSERT INTO performances (performance_id, work_id, show_id, director_id, performance_order)"
        ++ " VALUES($1, $2, $3, $4, $5)", [uuid, uuid, uuid, uuid, int8]),
    {ok, InsertOnstage} = epgsql:parse(C, "INSERT INTO performance_onstage (performance_id, performer_id, role, understudy_id, date_started, date_ended)"
        ++ " VALUES($1, $2, $3, $4, $5, $6)", [uuid, uuid, text, uuid, date, date]),
    {ok, InsertOffstage} = epgsql:parse(C, "INSERT INTO performance_offstage (performance_id, person_id, job, date_started, date_ended)"
        ++ " VALUES($1, $2, $3, $4, $5)", [uuid, uuid, text, date, date]),
    {ok, InsertShow} = epgsql:parse(C, "INSERT INTO shows (show_id, title, producing_org_id, special_thanks, date_created) "
        ++ " VALUES($1, $2, $3, $4, CURRENT_DATE)", [uuid, text, uuid, text, date]),
     State = #state{connection=C,
                   begin_statement=BeginStmt,
                   commit_statement=CommitStmt,
                   insert_work_statement=InsertWork,
                   insert_authorship_statement=InsertAuthorship,
                   insert_person_statement=InsertPerson,
                   insert_performance_statement=InsertPerformance,
                   insert_onstage_statement=InsertOnstage,
                   insert_offstage_statement=InsertOffstage,
                   insert_show_statement=InsertShow,
                   insert_org_statement=InsertOrg},
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #state{connection=C}) ->
    lager:error("DB server terminating: ~p", [Reason]),
    epgsql:close(C).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

initialize_tables(_Connection) ->
    ok.

get_offstage_inserts(Onstage, #state{insert_offstage_statement=IO}) ->
    PerformanceId = maps:get(performance_id, Onstage),
    PerformerId = maps:get(performer_id, Onstage),
    Job = maps:get(job, Onstage),
    DateStarted = maps:get(date_started, Onstage, null),
    DateEnded = maps:get(date_started, Onstage, null),

    Inserts = [{IO, [PerformanceId, PerformerId, Job, DateStarted, DateEnded]}],
    {Inserts, []}.


get_onstage_inserts(Onstage, #state{insert_onstage_statement=IO}) ->
    PerformanceId = maps:get(performance_id, Onstage),
    PerformerId = maps:get(performer_id, Onstage),
    Role = maps:get(role, Onstage),
    UnderstudyId = maps:get(understudy, Onstage, null),
    DateStarted = maps:get(date_started, Onstage, null),
    DateEnded = maps:get(date_started, Onstage, null),

    Inserts = [{IO, [PerformanceId, PerformerId, Role, UnderstudyId, DateStarted, DateEnded]}],
    {Inserts, []}.

%% TODO: Are maps the best for this, really? Proplists?
get_performance_inserts(PerformanceMap, State={insert_performance_statement=IP}) ->
    PerformanceId = uuid:uuid4(),
    ShowId = maps:get(show_id, PerformanceMap),
    WorkId = maps:get(work_id, PerformanceMap),
    Director = maps:get(director, PerformanceMap),
    {DirectorInserts, DirectorId} = get_person_inserts(Director, State),
    PerformanceOrder = maps:get(order, PerformanceMap),

    Inserts = lists:append([DirectorInserts,
                            [{IP, [PerformanceId, WorkId, ShowId, DirectorId, PerformanceOrder]}] ]),
    {Inserts, PerformanceId}.


get_show_inserts(ShowMap, State=#state{begin_statement=BEGIN,
                            commit_statement=COMMIT,
                            insert_show_statement=IS,
                            connection=C}) ->
    Work = maps:get(work, ShowMap),
    {WorkInserts, _WorkId} = get_work_inserts(Work, State),

    Org = maps:get(producing_org, ShowMap),
    {OrgInserts, OrgId} = get_producing_org_inserts(Org, State),

    ShowId = uuid:uuid4(),
    Title = maps:get(title, ShowMap),
    SpecialThanks = maps:get(special_thanks, ShowMap, ""),
    ShowInserts = [{IS, [ShowId, Title, OrgId, SpecialThanks]}],

    Batch = lists:append([[{BEGIN, []}],
                           WorkInserts,
                           OrgInserts,
                           ShowInserts,
                           [{COMMIT, []}]]),

    epgsql:execute_batch(C, Batch),
    ok.


get_producing_org_inserts(OrgMap, #state{insert_org_statement=IO}) ->
    OrgId = uuid:uuid4(),
    Name = maps:get(name, OrgMap),
    Parent = maps:get(parent, OrgMap, null),
    Tagline = maps:get(tagline, OrgMap, null),
    Description = maps:get(description, OrgMap, null),
    VanityName = maps:get(vanity_name, OrgMap, null),
    DateFounded = maps:get(parent, OrgMap, null),
    Visibility = maps:get(visibility, OrgMap, <<"public">>),

    OrgInserts = [{IO, [OrgId, Parent, Name, Tagline, Description, VanityName, DateFounded, Visibility]}],
    {OrgInserts, OrgId}.


get_work_inserts(WorkMap, State=#state{insert_work_statement=IW,
                                       insert_authorship_statement=IA}) ->
    WorkUUID = uuid:uuid4(),
    Title = maps:get(title, WorkMap, ""),
    Authors = maps:get(authors, WorkMap, []),

    {PersonInserts, Ids} = get_person_inserts(Authors, State),
    AuthorshipInserts = lists:map(fun (AuthorUUID) ->
                                      {IA, [WorkUUID, AuthorUUID]}
                              end, Ids),

    %% BEGIN, Insert the Work, Insert new People, Insert Authors, Commit
    WorkInserts = lists:append([ [{IW, [WorkUUID, Title, <<"public">>]}],
                                 PersonInserts,
                                 AuthorshipInserts]),
    {WorkInserts, [WorkUUID]}.

% -spec get_person_inserts(list({name | id, binary()}), #state{}) -> {list({#statement{}, list(any())}), binary()}.
get_person_inserts(PersonList, #state{insert_person_statement=IP}) ->
    PersonPairs = lists:map(fun ({Type, Value}) ->
                                  case Type of
                                      name -> 
                                          PersonUUID = uuid:uuid4(),
                                          {{IP, [PersonUUID, Value]}, PersonUUID};
                                      id ->
                                          {none, Value}
                                  end
                              end,  PersonList),
    {Inserts, Ids} = lists:unzip(PersonPairs),
    Filtered = lists:filter(fun(X) -> X =/= none end, Inserts),
    {Filtered, Ids}.

