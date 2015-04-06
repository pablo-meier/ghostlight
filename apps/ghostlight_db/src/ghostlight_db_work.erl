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
    lager:error("DB server (WORKS) terminating: ~p", [Reason]),
    epgsql:close(C).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(get_work_listings, _From, State=#db_state{connection=C, get_work_listings=GW}) ->
    epgsql:bind(C, GW, "", []),
    {ok, Rows} = epgsql:execute(C, GW),
    {reply, Rows, State};

handle_call({get_work, WorkId}, _From, State=#db_state{get_work_meta=GWM,
                                                       get_work_shows=GWS}) ->
    Batch = [ {GWM, [WorkId]},
              {GWS, [WorkId]} ],

    Reply = ghostlight_db_utils:exec_batch(Batch, State),
    {reply, Reply, State};

handle_call({insert_work, Work}, _From, State) ->
    {Inserts, WorkId} = get_inserts(Work, State),
    Reply = ghostlight_db_utils:exec_batch(Inserts, State),
    lager:info("Postgres responded to insert with ~p~n", [Reply]),
    {reply, WorkId, State};

handle_call({get_inserts, Work}, _From, State) ->
    Reply = get_inserts(Work, State),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.



%%%===================================================================
%%% Resource callbacks.
%%%===================================================================

get(WorkId) ->
    Response = gen_server:call(?MODULE, {get_work, WorkId}),
    [{ok, []}, {ok, Authors}, {ok, Shows}, {ok, []}] = Response,
    lager:info("Authors is ~p~n", [Authors]),
    [{WorkTitle, _, _, Description, MinutesLong, CollabOrgId, CollabOrgName}|_] = Authors,
    AuthorList = [ #person{ id = AuthorId, name = AuthorName } || {_, AuthorId, AuthorName, _, _, _, _} <- Authors ],
    ShowList = [ #show{
                     id=ShowId,
                     title=ShowTitle
                     %org=#organization{
                     %       id=OrgId,
                     %       name=OrgName
                     %    }
                 } || {ShowId, ShowTitle, _OrgId, _OrgName} <- Shows],

    CollabOrg = case CollabOrgId of
                    null ->
                        null;
                    _Else -> #organization{
                                    id=CollabOrgId,
                                    name=CollabOrgName }
                end,

    #work_return{
       work=#work{
               id=WorkId,
               title=WorkTitle,
               authors=AuthorList,
               description=Description,
               minutes_long=MinutesLong,
               collaborating_org=CollabOrg
            },
       shows=ShowList
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

get_inserts(#work{title=Title,
                  authors=Authors,
                  description=Description,
                  collaborating_org=Org,
                  minutes_long=MinutesLong},
            #db_state{insert_work_statement=IW,
                      insert_authorship_statement=IA}) ->
    WorkUUID = ghostlight_db_utils:fresh_uuid(),
    {PersonInsertsNested, Ids} = lists:unzip([ ghostlight_db_person:get_inserts(Author) || Author <- Authors ]),
    PersonInserts = lists:flatten(PersonInsertsNested),
    AuthorshipInserts = [ {IA, [WorkUUID, AuthorUUID]} || AuthorUUID <- Ids ],
    {OrgInserts, OrgId} = case Org of 
                              null -> {[], null};
                              _ -> ghostlight_db_org:get_inserts(Org)
                          end,

    Markdowned = ghostlight_db_utils:markdown_or_null(Description),

    WorkInserts = lists:append([ OrgInserts,
                                 [{IW, [WorkUUID, Title, Description, Markdowned, OrgId, MinutesLong, <<"public">>]}],
                                 PersonInserts,
                                 AuthorshipInserts]),
    {WorkInserts, WorkUUID}.

prepare_statements(C, State) ->
    WorksSql = "INSERT INTO works (work_id, title, description_src, description_markdown, collaborating_org_id, minutes_long, acl) VALUES($1, $2, $3, $4, $5, $6, $7)", 
    {ok, InsertWork} = epgsql:parse(C, "insert_work", WorksSql, [uuid, text, text, text, uuid, int8, text]),

    AuthorshipSql = "INSERT INTO authorship (work_id, person_id) VALUES($1, $2)",
    {ok, InsertAuthorship} = epgsql:parse(C, "insert_authorship", AuthorshipSql, [uuid, uuid]),

    GetWorkTitleAndAuthorsSql = "SELECT w.title, p.person_id, p.name, w.description_markdown, w.minutes_long, o.org_id, o.name "
        ++ "FROM works AS w INNER JOIN authorship AS a USING (work_id) "
        ++ "INNER JOIN people AS p USING (person_id) "
        ++ "LEFT OUTER JOIN organizations AS o ON (w.collaborating_org_id = o.org_id) "
        ++ "WHERE a.work_id = $1",
    {ok, GetWorkTitleAndAuthors} = epgsql:parse(C, "get_work_meta", GetWorkTitleAndAuthorsSql, [uuid]),

    GetWorkShowsSql = "SELECT s.show_id, s.title, o.org_id, o.name AS org_name "
        ++ "FROM shows AS s "
        ++ "INNER JOIN producers AS prod USING (show_id) "
        ++ "INNER JOIN performances AS p USING (show_id) "
        ++ "INNER JOIN organizations AS o USING (org_id) "
        ++ "INNER JOIN works AS w ON (p.work_id = w.work_id) WHERE w.work_id = $1",
    {ok, GetWorkShows} = epgsql:parse(C, "get_work_shows", GetWorkShowsSql, [uuid]),
        
    %% For work listings -- much like the meta of a single one.
    GetWorkListingsSql = "SELECT w.work_id, w.title, p.person_id, p.name FROM works AS w "
        ++ "INNER JOIN authorship AS a using (work_id) INNER JOIN people AS p USING (person_id) ORDER BY w.title ASC LIMIT 50",
    {ok, GetWorkListings} = epgsql:parse(C, "get_work_listings", GetWorkListingsSql, []),


    State#db_state{
       insert_work_statement=InsertWork,
       insert_authorship_statement=InsertAuthorship,

       get_work_meta=GetWorkTitleAndAuthors,
       get_work_shows=GetWorkShows,

       get_work_listings=GetWorkListings
    }.

