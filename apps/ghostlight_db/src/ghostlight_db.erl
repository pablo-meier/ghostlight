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
         get_show_listings/0,
         insert_show/1,

         get_org/1,
         get_org/2,
         update_org/1,
         get_org_listings/0,
         insert_org/1,

         get_person/1,
         get_person_listings/0,
         insert_person/1,

         get_work/1,
         get_work_listings/0,
         insert_work/1
        ]).

-export([fix_dups/0,
         exec_batch/1]).

-include("apps/ghostlight/include/ghostlight_data.hrl").

-define(SERVER, ?MODULE).
-record(state, {connection,
                begin_statement,
                commit_statement,

                person_de_dupes,
                org_de_dupes
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
                                          org_de_dupes=OrgDeDupes}) ->
    {ok, _Columns, PeopleRows} = epgsql:squery(C, "SELECT id1, id2, name FROM (SELECT p1.person_id AS id1, p2.person_id AS id2, "
        ++ "p1.name, row_number() OVER (PARTITION BY p1.name) AS row_num FROM people AS p1, people AS p2 "
        ++ "WHERE p1.name = p2.name AND p1.person_id != p2.person_id ORDER BY p1.name) AS tmp WHERE row_num < 2"),

    lists:foreach(fun ({GoodId, BadId, Name}) ->
                     Statements = [ {Parsed, [GoodId, BadId]} || Parsed <- PplDeDupes ],
                     epgsql:execute_batch(C, lists:append([ [{BEGIN, []}], Statements, [{COMMIT, []}] ])),
                     lager:info("Swapped out ~p for ~p for user ~p", [BadId, GoodId, Name])
                  end, PeopleRows),

    {ok, _Columns, OrgRows} = epgsql:squery(C, "SELECT id1, id2, name FROM (SELECT o1.org_id AS id1, o2.org_id AS id2, "
        ++ "o1.name, row_number() OVER (PARTITION BY o1.name) AS row_num FROM organizations AS o1, organizations AS o2 "
        ++ "WHERE o1.name = o2.name AND o1.org_id != o2.org_id ORDER BY o1.name) AS tmp WHERE row_num < 2"),

    lists:foreach(fun ({GoodId, BadId, Name}) ->
                     lager:info("Swapping out ~p for ~p for org ~p", [BadId, GoodId, Name]),
                     Statements = [ {Parsed, [GoodId, BadId]} || Parsed <- OrgDeDupes ],
                     epgsql:execute_batch(C, lists:append([ [{BEGIN, []}], Statements, [{COMMIT, []}] ]))
                  end, OrgRows),

    {reply, ok, State};

handle_call({exec_batch, Batch},
            _From,
            State=#state{connection=C,
                         commit_statement=COMMIT,
                         begin_statement=BEGIN}) ->
    AsTransaction = lists:append([ [{BEGIN, []}],
                                   Batch,
                                   [{COMMIT, []}] ]),
    Results = epgsql:execute_batch(C, AsTransaction),
    {reply, Results, State};

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
    ghostlight_db_show:insert(Show).
get_show_listings() ->
    ghostlight_db_show:listings().
get_show(ShowId) ->
    ghostlight_db_show:get(ShowId).

insert_org(Org) ->
    ghostlight_db_org:insert(Org).
get_org_listings() ->
    ghostlight_db_org:listings().
get_org(OrgId) ->
    ghostlight_db_org:get(OrgId).
get_org(OrgId, Form) ->
    ghostlight_db_org:get(OrgId, Form).
update_org(Org) ->
    ghostlight_db_org:update(Org).

get_work(WorkId) ->
    ghostlight_db_work:get(WorkId).
get_work_listings() ->
    ghostlight_db_work:listings().
insert_work(Work) ->
    ghostlight_db_work:insert(Work).

get_person(PersonId) ->
    ghostlight_db_person:get(PersonId).
get_person_listings() ->
    ghostlight_db_person:listings().
insert_person(Person) ->
    ghostlight_db_person:insert(Person).

%%%===================================================================
%%% Internal functions
%%%===================================================================

exec_batch(Batch) ->
    gen_server:call(?MODULE, {exec_batch, Batch}).


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
    {ok, OrgParsed7} = epgsql:parse(C, "consolidate_g", "UPDATE works SET collaborating_org_id = $1 WHERE collaborating_org_id = $2", [uuid, uuid]),
    {ok, OrgParsed8} = epgsql:parse(C, "consolidate_h", "DELETE FROM organizations WHERE org_id = $2", [uuid]),
    OrgDeDupes = [OrgParsed1, OrgParsed2, OrgParsed3, OrgParsed4, OrgParsed5,
                  OrgParsed6, OrgParsed7, OrgParsed8],
 
    State = #state{connection=C,
                   begin_statement=BeginStmt,
                   commit_statement=CommitStmt,
                   person_de_dupes=PplDeDupes,
                   org_de_dupes=OrgDeDupes
                   },
    State.

%%% SUPER SECRET FUNCTIONS
fix_dups() ->
    gen_server:call(?MODULE, fix_dups).
