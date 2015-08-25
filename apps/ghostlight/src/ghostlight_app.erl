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
    Return = ghostlight_sup:start_link(),
    initiate_listening_to_endpoints(),
    compile_all_templates(),
    register_healthchecks(),
    Return.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

initiate_listening_to_endpoints() ->
    Port = ghostlight_config:get(ghostlight_port),
    Dispatch = cowboy_router:compile([
                                      {'_', [
                                             {"/static/[...]", cowboy_static, {priv_dir, ghostlight, "static/",
                                                                               [{mimetypes, cow_mimetypes, all}]}},
                                             {"/robots.txt", cowboy_static, {priv_file, ghostlight, "static/robots.txt"}},
                                             {"/humans.txt", cowboy_static, {priv_file, ghostlight, "static/humans.txt"}},
                                             {"/favicon.ico", cowboy_static, {priv_file, ghostlight, "static/favicon.ico"}},

                                             {"/", ghostlight_static, []},
                                             {"/index.html", ghostlight_static, []},
                                             {"/about.html", ghostlight_static, []},
                                             {"/faq.html", ghostlight_static, []},
                                             {"/search", ghostlight_search, []},
                                             {"/:resource[/:resource_id[/:command]]", ghostlight_resource, []}
                                      ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
                                                            {env, [{dispatch, Dispatch}]},
                                                            {middlewares, [cowboy_router, ghostlight_logging, cowboy_handler]},
                                                            {onresponse, fun ghostlight_utils:handle_errors/4}
    ]),
    lager:info("Started the Ghostlight server on Port ~p~n", [Port]).


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
                    case erlydtl:compile_file(FileAbsName, list_to_atom(ModuleName), [{out_dir, false},
                                                                                      {return_errors, true}]) of
                        {ok, _} ->
                            lager:info("    Success");
                        Else ->
                            lager:error("    Error: ~p~n", [Else])
                    end
                  end, Filenames),
    ok.

register_healthchecks() ->
    ghostlight_healthchecks:register(ghostlight_db, healthcheck, <<"Postgres Connectivity">>),
    ghostlight_healthchecks:register(ghostlight_markdown, healthcheck, <<"Markdown parser">>),
    ghostlight_healthchecks:register(ghostlight_sanitizer, healthcheck, <<"HTML sanitizer">>).
