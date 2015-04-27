%%% Welcome to the New World!
%%% 
%%% Here, we keep SQL in SQL files kept in `priv/`, not awkwardly expressed in
%%% Erlang terms. Each resource gets its own connection to the DB (or pool of 
%%% them, eventually).
%%%
%%% I can maybe abstract out how each resource can be made into a module with
%%% callbacks, like gen_server does or Cowboy handlers, but for now I'll just
%%% handle the gen_servers myself.
%%%
%%% How to do that:
%%% * How do I gen_server:call?
%%% * Who supervises?
%%%
%%% My Erlang-foo needs work.
-module(ghostlight_db_work).
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
         update/1,
         
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
    lager:error("DB server (WORKS) terminating: ~p", [Reason]),
    epgsql:close(C).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(get_work_listings, _From, State=#db_state{connection=C, get_work_listings=GW}) ->
    epgsql:bind(C, GW, "", []),
    {ok, Rows} = epgsql:execute(C, GW),
    {reply, Rows, State};

handle_call({get_work, WorkId}, _From, State=#db_state{get_work_statement=GW}) ->
    Batch = [ {GW, [WorkId]} ],
    Reply = ghostlight_db_utils:exec_batch(Batch, State),
    {reply, Reply, State};

handle_call({insert_work, Work}, _From, State) ->
    {Inserts, WorkId} = get_inserts(Work, State),
    Reply = ghostlight_db_utils:exec_batch(Inserts, State),
    lager:info("Postgres responded to insert with ~p~n", [Reply]),
    {reply, WorkId, State};

handle_call({update_work, Work}, _From, State) ->
    Commands  = get_update_commands(Work, State),
    Reply = ghostlight_db_utils:exec_batch(Commands, State),
    lager:info("Postgres responded to update with ~p~n", [Reply]),
    {reply, true, State};

handle_call({get_inserts, Work}, _From, State) ->
    Reply = get_inserts(Work, State),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


get_update_commands(#work{id=WorkId,
                          title=Title,
                          description=DescSrc,
                          collaborating_org=CollabOrg,
                          minutes_long=MinutesLong,
                          authors=Authors},
                    #db_state{update_work_statement=UW,
                              delete_authors_statement=DA,
                              insert_authorship_statement=IA}) ->

    DescMarkdown = ghostlight_db_utils:markdown_or_null(DescSrc),
    AuthorshipInserts = lists:flatten([ get_author_inserts(WorkId, Author, IA) || Author <- Authors ]),

    {CollabOrgInserts, CollabOrgId} = case CollabOrg of
                                          null -> {[], null};
                                          #organization{} -> ghostlight_db_org:get_inserts(CollabOrg)
                                      end,

    lager:info("WorkId is ~p~n", [WorkId]),
    lager:info("CInserts ~p~n, OrgId ~p~n", [CollabOrgInserts, CollabOrgId]),

    lists:append([ CollabOrgInserts,
                   [{UW, [Title, DescSrc, DescMarkdown, CollabOrgId, MinutesLong, WorkId]},
                    {DA, [WorkId]}],
                   AuthorshipInserts]).


%%%===================================================================
%%% Resource callbacks.
%%%===================================================================

get(WorkId) ->
  get(WorkId, html).

get(WorkId, Format) ->
    Response = gen_server:call(?MODULE, {get_work, WorkId}),
    [{ok, []},
     {ok, [{
        WorkId,
        Title,
        CollaboratingOrgs,
        Authors,
        DescriptionSrc,
        DescriptionMarkdown,
        MinutesLong,
        Productions
       }]},
     {ok, []}] = Response,

    Description = case Format of html -> DescriptionMarkdown; markdown -> DescriptionSrc end,

    #work_return{
       work=#work{
               id=WorkId,
               title=Title,
               authors=[ ghostlight_db_utils:parse_person_or_org(Author) || Author <- jiffy:decode(Authors) ],
               description=Description,
               minutes_long=MinutesLong,
               collaborating_org=[ parse_org(Org) || Org <- jiffy:decode(CollaboratingOrgs) ]
            },
       shows=[ parse_show(Show) || Show <- jiffy:decode(Productions) ]
    }.

parse_org({Org}) ->
    #organization{
       id = proplists:get_value(<<"org_id">>, Org),
       name = proplists:get_value(<<"name">>, Org)
    }.

parse_show({Show}) ->
    #show{
       id = proplists:get_value(<<"show_id">>, Show),
       title = proplists:get_value(<<"title">>, Show),
       producers = [ ghostlight_db_utils:parse_person_or_org(Producer) ||
                     Producer <- proplists:get_value(<<"producers">>, Show)]
    }.

listings() ->
    Response = gen_server:call(?MODULE, get_work_listings),
    AuthorMap = lists:foldl(fun({Id, Title, AuthorId, AuthorName}, Accum) ->
                                Author = #person{
                                            id=AuthorId,
                                            name=AuthorName
                                         },
                                case maps:get({Id, Title}, Accum, undefined) of
                                    undefined ->
                                        maps:put({Id, Title}, [Author], Accum);
                                    AuthorList ->
                                        maps:put({Id, Title}, [Author|AuthorList], Accum)
                                end
                            end, maps:new(), Response),
    [ #work{
          id=WorkId,
          title=WorkTitle,
          authors=maps:get({WorkId, WorkTitle}, AuthorMap)
      } || {WorkId, WorkTitle} <- maps:keys(AuthorMap)].

insert(Work) ->
    gen_server:call(?MODULE, {insert_work, Work}).

get_inserts(Work) ->
    gen_server:call(?MODULE, {get_inserts, Work}).

update(Work) ->
    gen_server:call(?MODULE, {update_work, Work}).

get_inserts(#work{title=Title,
                  authors=Authors,
                  description=Description,
                  collaborating_org=Org,
                  minutes_long=MinutesLong},
            #db_state{insert_work_statement=IW,
                      insert_authorship_statement=IA}) ->
    WorkUUID = ghostlight_db_utils:fresh_uuid(),
    AuthorshipInserts = lists:flatten([ get_author_inserts(WorkUUID, Author, IA) || Author <- Authors ]),
    {OrgInserts, OrgId} = case Org of 
                              null -> {[], null};
                              _ -> ghostlight_db_org:get_inserts(Org)
                          end,

    Markdowned = ghostlight_db_utils:markdown_or_null(Description),

    WorkInserts = lists:append([ OrgInserts,
                                 [{IW, [WorkUUID, Title, Description, Markdowned, OrgId, MinutesLong, <<"public">>]}],
                                 AuthorshipInserts]),
    {WorkInserts, WorkUUID}.

get_author_inserts(WorkId, Person=#person{}, Stmt) ->
    {PersonInserts, PersonId} = ghostlight_db_person:get_inserts(Person),
    lists:append([ PersonInserts,
                   [{Stmt, [WorkId, PersonId, null]}] ]);
get_author_inserts(WorkId, Org=#organization{}, Stmt) ->
    {OrgInserts, OrgId} = ghostlight_db_org:get_inserts(Org),
    lists:append([ OrgInserts,
                   [{Stmt, [WorkId, null, OrgId]}] ]).


prepare_statements(C, State) ->
    WorksSql = "INSERT INTO works (work_id, title, description_src, description_markdown, collaborating_org_id, minutes_long, acl) VALUES($1, $2, $3, $4, $5, $6, $7)", 
    {ok, InsertWork} = epgsql:parse(C, "insert_work", WorksSql, [uuid, text, text, text, uuid, int8, text]),
    AuthorshipSql = "INSERT INTO authorship (work_id, person_id, org_id) VALUES($1, $2, $3)",
    {ok, InsertAuthorship} = epgsql:parse(C, "insert_authorship", AuthorshipSql, [uuid, uuid, uuid]),

    GetWorkSql =
"
SELECT
    w.work_id,
    w.title,
    array_to_json(ARRAY(SELECT (o.org_id, o.name)::org_pair 
                        FROM organizations o
                        WHERE o.org_id = w.collaborating_org_id)) AS collaborating_org,
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
    GetWorkListingsSql = "SELECT w.work_id, w.title, p.person_id, p.name FROM works AS w "
        ++ "INNER JOIN authorship AS a using (work_id) INNER JOIN people AS p USING (person_id) ORDER BY w.title ASC LIMIT 50",
    {ok, GetWorkListings} = epgsql:parse(C, "get_work_listings", GetWorkListingsSql, []),

    UpdateWorkSql = "UPDATE works SET title = $1, description_src = $2, description_markdown = $3, collaborating_org_id = $4, minutes_long = $5 WHERE work_id = $6",
    {ok, UpdateWork} = epgsql:parse(C, "update_work", UpdateWorkSql, [text, text, text, uuid, int8, uuid]),
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

