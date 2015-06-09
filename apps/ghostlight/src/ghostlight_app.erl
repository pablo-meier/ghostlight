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
    register_resources(),
    initiate_listening_to_endpoints(),
    compile_all_templates(),
    Return.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================


register_resources() ->
    Resources = 
        [
         [{resource_name, "people"},
          {module, ghostlight_people},
          {get_listing_template, person_listing_template},
          {get_template, person_template},
          {update_template, insert_person_template}]
        ],
    lists:foreach(fun ghostlight_resource:register/1, Resources),
    ok.


initiate_listening_to_endpoints() ->
    Port = ghostlight_config:get(ghostlight_port),
    Dispatch = cowboy_router:compile([
                                      {'_', [
                                             {"/static/[...]", cowboy_static, {priv_dir, ghostlight, "static/",
                                                                               [{mimetypes, cow_mimetypes, all}]}},
                                             {"/faq", cowboy_static, {priv_file, ghostlight, "static/faq.html"}},
                                             {"/favicon.ico", cowboy_static, {priv_file, ghostlight, "static/favicon.ico"}},
                                             {"/index.html", cowboy_static, {priv_file, ghostlight, "static/homepage.html"}},
                                             {"/", cowboy_static, {priv_file, ghostlight, "static/homepage.html"}},
                                             {"/works/[:work_id[/:command]]", ghostlight_work, []},
                                             {"/shows/[:show_id[/:command]]", ghostlight_show, []},
                                             {"/organizations/[:org_id[/:command]]", ghostlight_org, []},
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
