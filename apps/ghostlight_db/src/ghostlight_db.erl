-module(ghostlight_db).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([
         get_show/1,
         get_show/2,
         update_show/1,
         get_show_listings/0,
         insert_show/1,

         get_org/1,
         get_org/2,
         update_org/1,
         get_org_listings/0,
         insert_org/1,

         get_person/1,
         get_person/2,
         get_person_listings/0,
         insert_person/1,
         update_person/1,

         get_work/1,
         get_work/2,
         get_work_listings/0,
         insert_work/1,
         update_work/1,

         resolve_vanity/2,

         healthcheck/0
        ]).

-export([fix_dups/0]).

-define(SERVER, ?MODULE).
-record(state, {connection,
                begin_statement,
                commit_statement,

                person_de_dupes,
                org_de_dupes,
                work_de_dupes,

                show_get_vanity,
                org_get_vanity,
                person_get_vanity,
                work_get_vanity
                }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    C = ghostlight_db_utils:connect_to_postgres(),
    State = prepare_statements(C),
    {ok, State}.


handle_call(fix_dups, _From, State=#state{connection=C,
                                          begin_statement=BEGIN,
                                          commit_statement=COMMIT,
                                          person_de_dupes=PplDeDupes,
                                          org_de_dupes=OrgDeDupes,
                                          work_de_dupes=WorkDeDupes}) ->
    {ok, _, PeopleRows} = epgsql:squery(C, "SELECT id1, id2, name FROM (SELECT p1.person_id AS id1, p2.person_id AS id2, "
        ++ "p1.name, row_number() OVER (PARTITION BY p1.name) AS row_num FROM people AS p1, people AS p2 "
        ++ "WHERE p1.name = p2.name AND p1.person_id != p2.person_id ORDER BY p1.name) AS tmp WHERE row_num < 2"),

    lists:foreach(fun ({GoodId, BadId, Name}) ->
                     Statements = [ {Parsed, [GoodId, BadId]} || Parsed <- PplDeDupes ],
                     epgsql:execute_batch(C, lists:append([ [{BEGIN, []}], Statements, [{COMMIT, []}] ])),
                     lager:info("Swapped out ~p for ~p for user ~p", [BadId, GoodId, Name])
                  end, PeopleRows),

    {ok, _, OrgRows} = epgsql:squery(C, "SELECT id1, id2, name FROM (SELECT o1.org_id AS id1, o2.org_id AS id2, "
        ++ "o1.name, row_number() OVER (PARTITION BY o1.name) AS row_num FROM organizations AS o1, organizations AS o2 "
        ++ "WHERE o1.name = o2.name AND o1.org_id != o2.org_id ORDER BY o1.name) AS tmp WHERE row_num < 2"),

    lists:foreach(fun ({GoodId, BadId, Name}) ->
                     lager:info("Swapping out ~p for ~p for org ~p", [BadId, GoodId, Name]),
                     Statements = [ {Parsed, [GoodId, BadId]} || Parsed <- OrgDeDupes ],
                     epgsql:execute_batch(C, lists:append([ [{BEGIN, []}], Statements, [{COMMIT, []}] ]))
                  end, OrgRows),

    {ok, _, WorkRows} = epgsql:squery(C, "SELECT id1, id2, title FROM (SELECT w1.work_id AS id1, w2.work_id AS id2, "
        ++ "w1.title, row_number() OVER (PARTITION BY w1.title) AS row_num FROM works AS w1, works AS w2 "
        ++ "WHERE w1.title = w2.title AND w1.work_id != w2.work_id ORDER BY w1.title) AS tmp WHERE row_num < 2"),

    lists:foreach(fun ({GoodId, BadId, Name}) ->
                     lager:info("Swapping out ~p for ~p for work ~p", [BadId, GoodId, Name]),
                     Statements = [ {Parsed, [GoodId, BadId]} || Parsed <- WorkDeDupes ],
                     epgsql:execute_batch(C, lists:append([ [{BEGIN, []}], Statements, [{COMMIT, []}] ]))
                  end, WorkRows),

    {reply, ok, State};

handle_call({vanity, Resource, Name},
            _From,
            State=#state{connection=C,
                         show_get_vanity=ShowGetVanity,
                         org_get_vanity=OrgGetVanity,
                         person_get_vanity=PersonGetVanity,
                         work_get_vanity=WorkGetVanity}) ->

    Stmt = case Resource of
               shows -> ShowGetVanity;
               organizations -> OrgGetVanity;
               people -> PersonGetVanity;
               works -> WorkGetVanity
           end,

    Response = case epgsql:execute_batch(C, [{Stmt, [Name]}]) of
                   [{ok, []}] -> throw(not_found);
                   [{ok, [{Id}]}] -> Id;
                   _ -> error
               end,
    {reply, Response, State};


handle_call(healthcheck, _, State=#state{connection=C}) ->
    Reply = epgsql:squery(C, "SELECT 1=1"),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #state{connection=C}) ->
    lager:error("DB server terminating: ~p", [Reason]),
    epgsql:close(C).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Smooth-n-easy API for the normies
%%%===================================================================

insert_show(Show) ->
    ghostlight_db_resource:insert(ghostlight_db_show, Show).
get_show_listings() ->
    ghostlight_db_resource:listings(ghostlight_db_show).
get_show(ShowId) ->
    ghostlight_db_resource:get(ghostlight_db_show, ShowId).
get_show(ShowId, Form) ->
    ghostlight_db_resource:get(ghostlight_db_show, ShowId, Form).
update_show(ShowId) ->
    ghostlight_db_resource:update(ghostlight_db_show, ShowId).

get_org(OrgId) ->
    ghostlight_db_resource:get(ghostlight_db_org, OrgId).
get_org(OrgId, Form) ->
    ghostlight_db_resource:get(ghostlight_db_org, OrgId, Form).
insert_org(Org) ->
    ghostlight_db_resource:insert(ghostlight_db_org, Org).
get_org_listings() ->
    ghostlight_db_resource:listings(ghostlight_db_org).
update_org(Org) ->
    ghostlight_db_resource:update(ghostlight_db_org, Org).

get_work(WorkId) ->
    ghostlight_db_resource:get(ghostlight_db_work, WorkId).
get_work(WorkId, Form) ->
    ghostlight_db_resource:get(ghostlight_db_work, WorkId, Form).
insert_work(Work) ->
    ghostlight_db_resource:insert(ghostlight_db_work, Work).
get_work_listings() ->
    ghostlight_db_resource:listings(ghostlight_db_work).
update_work(Work) ->
    ghostlight_db_resource:update(ghostlight_db_work, Work).

get_person(PersonId) ->
    ghostlight_db_resource:get(ghostlight_db_person, PersonId).
get_person(PersonId, Form) ->
    ghostlight_db_resource:get(ghostlight_db_person, PersonId, Form).
get_person_listings() ->
    ghostlight_db_resource:listings(ghostlight_db_person).
insert_person(Person) ->
    ghostlight_db_resource:insert(ghostlight_db_person, Person).
update_person(Person) ->
    ghostlight_db_resource:update(ghostlight_db_person, Person).

resolve_vanity(_, undefined) -> undefined;
resolve_vanity(Resource, Id) ->
    case ghostlight_db_utils:is_valid_uuid(Id) of
        true -> Id;
        false -> resolve_resource_vanity(Resource, Id)
    end.

resolve_resource_vanity(people, Name) -> gen_server:call(?MODULE, {vanity, people, Name});
resolve_resource_vanity(organizations, Name) -> gen_server:call(?MODULE, {vanity, organizations, Name});
resolve_resource_vanity(shows, Name) -> gen_server:call(?MODULE, {vanity, shows, Name});
resolve_resource_vanity(works, Name) -> gen_server:call(?MODULE, {vanity, works, Name}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

prepare_statements(C) ->
    {ok, BeginStmt} = epgsql:parse(C, "begin_statement", "BEGIN", []),
    {ok, CommitStmt} = epgsql:parse(C, "commit_statement", "COMMIT", []),

    %% De-duping
    {ok, Parsed1} = epgsql:parse(C, "consolidate_1", "UPDATE authorship SET person_id = $1 WHERE person_id = $2", [uuid, uuid]),
    {ok, Parsed2} = epgsql:parse(C, "consolidate_2", "UPDATE org_employees SET person_id = $1 WHERE person_id = $2", [uuid, uuid]),
    {ok, Parsed3} = epgsql:parse(C, "consolidate_3", "UPDATE org_members SET person_id = $1 WHERE person_id = $2", [uuid, uuid]),
    {ok, Parsed4} = epgsql:parse(C, "consolidate_4", "UPDATE people_links SET person_id = $1 WHERE person_id = $2", [uuid, uuid]),
    {ok, Parsed5} = epgsql:parse(C, "consolidate_5", "UPDATE performance_directors SET director_id = $1 WHERE director_id = $2", [uuid, uuid]),
    {ok, Parsed6} = epgsql:parse(C, "consolidate_6", "UPDATE performance_offstage SET person_id = $1 WHERE person_id = $2", [uuid, uuid]),
    {ok, Parsed7} = epgsql:parse(C, "consolidate_7", "UPDATE performance_onstage SET performer_id = $1 WHERE performer_id = $2", [uuid, uuid]),
    {ok, Parsed8} = epgsql:parse(C, "consolidate_8", "UPDATE performance_onstage SET understudy_id = $1 WHERE understudy_id = $2", [uuid, uuid]),
    {ok, Parsed9} = epgsql:parse(C, "consolidate_9", "UPDATE producers SET person_id = $1 WHERE person_id = $2", [uuid, uuid]),
    {ok, Parsed10} = epgsql:parse(C, "consolidate_10", "UPDATE show_hosts SET person_id = $1 WHERE person_id = $2", [uuid, uuid]),
    {ok, Parsed11} = epgsql:parse(C, "consolidate_11", "UPDATE users SET person_id = $1 WHERE person_id = $2", [uuid, uuid]),
    {ok, Parsed12} = epgsql:parse(C, "consolidate_12", "DELETE FROM people WHERE person_id = $2", [uuid]),
    PplDeDupes = [Parsed1, Parsed2, Parsed3, Parsed4, Parsed5, Parsed6, Parsed7,
                  Parsed8, Parsed9, Parsed10, Parsed11, Parsed12],

    {ok, OrgParsed1} = epgsql:parse(C, "consolidate_a", "UPDATE authorship SET org_id = $1 WHERE org_id = $2", [uuid, uuid]),
    {ok, OrgParsed2} = epgsql:parse(C, "consolidate_b", "UPDATE org_employees SET org_id = $1 WHERE org_id = $2", [uuid, uuid]),
    {ok, OrgParsed3} = epgsql:parse(C, "consolidate_c", "UPDATE org_links SET org_id = $1 WHERE org_id = $2", [uuid, uuid]),
    {ok, OrgParsed4} = epgsql:parse(C, "consolidate_d", "UPDATE org_members SET org_id = $1 WHERE org_id = $2", [uuid, uuid]),
    {ok, OrgParsed5} = epgsql:parse(C, "consolidate_e", "UPDATE performance_offstage SET org_id = $1 WHERE org_id = $2", [uuid, uuid]),
    {ok, OrgParsed6} = epgsql:parse(C, "consolidate_f", "UPDATE producers SET org_id = $1 WHERE org_id = $2", [uuid, uuid]),
    {ok, OrgParsed7} = epgsql:parse(C, "consolidate_h", "DELETE FROM organizations WHERE org_id = $2", [uuid]),
    OrgDeDupes = [OrgParsed1, OrgParsed2, OrgParsed3, OrgParsed4, OrgParsed5,
                  OrgParsed6, OrgParsed7],

    {ok, WorkParsed1} = epgsql:parse(C, "consolidate_i", "DELETE FROM authorship WHERE work_id = $2", [uuid]),
    {ok, WorkParsed2} = epgsql:parse(C, "consolidate_ii", "UPDATE performances SET work_id = $1 WHERE work_id = $2", [uuid, uuid]),
    {ok, WorkParsed3} = epgsql:parse(C, "consolidate_iii", "DELETE FROM works WHERE work_id = $2", [uuid]),
    WorkDeDupes = [WorkParsed1, WorkParsed2, WorkParsed3],
    
    {ok, ShowGetVanity} = epgsql:parse(C, "show_vanity", "SELECT show_id FROM shows WHERE vanity_name = $1", [text]),
    {ok, OrgGetVanity} = epgsql:parse(C, "org_vanity", "SELECT org_id FROM organizations WHERE vanity_name = $1", [text]),
    {ok, PersonGetVanity} = epgsql:parse(C, "person_vanity", "SELECT person_id FROM users WHERE vanity_name = $1", [text]),
    {ok, WorkGetVanity} = epgsql:parse(C, "work_vanity", "SELECT work_id FROM works WHERE vanity_name = $1", [text]),

    State = #state{connection=C,
                   begin_statement=BeginStmt,
                   commit_statement=CommitStmt,
                   person_de_dupes=PplDeDupes,
                   org_de_dupes=OrgDeDupes,
                   work_de_dupes=WorkDeDupes,

                   show_get_vanity=ShowGetVanity,
                   org_get_vanity=OrgGetVanity,
                   person_get_vanity=PersonGetVanity,
                   work_get_vanity=WorkGetVanity
                   },
    State.

healthcheck() ->
    case gen_server:call(?MODULE, healthcheck) of
        {ok, _, _} -> ok;
        Else -> Else
    end.

%%% SUPER SECRET FUNCTIONS
fix_dups() ->
    gen_server:call(?MODULE, fix_dups).
