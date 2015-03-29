%%%-------------------------------------------------------------------
%% @doc ghostlight public API
%% @end
%%%-------------------------------------------------------------------

-module(ghostlight_app).

-behaviour(application).
-export([start/2, stop/1]).  

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    lager:start(),
    lager:info("__~-~-*^*HELLO GHOSTLIGHT*^*-~-~__"),
    initiate_listening_to_endpoints(),
    compile_all_templates(),
    application:start(ghostlight_db),
    ghostlight_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================


initiate_listening_to_endpoints() ->
    Port = 8080,
    Dispatch = cowboy_router:compile([
                                      {'_', [
                                             {"/shows/[:show_id]", ghostlight_show, []},
                                             {"/people/[:person_id]", ghostlight_people, []},
                                             {"/organizations/[:org_id]", ghostlight_org, []},
                                             {"/works/[:work_id]", ghostlight_work, []},
                                             {"/static/[...]", cowboy_static, {priv_dir, ghostlight, "static/",
                                                                               [{mimetypes, cow_mimetypes, all}]}}
                                      ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
                                                            {env, [{dispatch, Dispatch}]},
                                                            {middlewares, [cowboy_router, ghostlight_logging, cowboy_handler]}
    ]),
    lager:info("Started the Cowboy server on Port ~p~n", [Port]).


%% Compiles ErlyDtl templates the other modules will use. If it fails, better
%% to have it happen at this stage.
compile_all_templates() ->
    PrivDir = code:priv_dir(ghostlight),
    Templates = filename:join([PrivDir, "templates"]),
    Native = filename:nativename(Templates),
    {ok, DirContents} = file:list_dir(Native),
    Filenames = lists:filter(fun (X) -> re:run(X, "^\\.") == nomatch end, DirContents),
    lager:info("Compiling Templates..."),
    lists:foreach(fun (File) ->
                    ModuleName = filename:basename(File, ".html") ++ "_template",
                    FileAbsName = filename:join([Native, File]),
                    lager:info("  Compiling a template module named ~p~n", [ModuleName]),
                    case erlydtl:compile_file(FileAbsName, list_to_atom(ModuleName), [{out_dir, false}, {return_errors, true}]) of
                        {ok, _} ->
                            lager:info("    Success");
                        Else ->
                            lager:error("    Error: ~p~n", [Else])
                    end
                  end, Filenames),
    ok.
