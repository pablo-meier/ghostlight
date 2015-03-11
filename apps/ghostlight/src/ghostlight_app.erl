%%%-------------------------------------------------------------------
%% @doc ghostlight public API
%% @end
%%%-------------------------------------------------------------------

-module(ghostlight_app).

-behaviour(application).
%-compile([{compile_transform, lager_transform}]).
%% Application callbacks
-export([start/2, stop/1]).  

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    lager:start(),
    lager:info("HELLO GHOSTLIGHT"),
    initiate_listening_to_endpoints(),
    compile_all_templates(),
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
                                             {"/shows", ghostlight_show, []},
                                             {"/static/[...]", cowboy_static, {priv_dir, ghostlight, "static/",
                                                                               [{mimetypes, cow_mimetypes, all}]}}
                                      ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
                                                            {env, [{dispatch, Dispatch}]}
    ]),
    lager:info("Started the Cowboy server on Port ~p~n", [Port]).


%% Compiles ErlyDtl templates the other modules will use. If it fails, better
%% to have it happen at this stage.
compile_all_templates() ->
    PrivDir = code:priv_dir(ghostlight),
    Templates = filename:join([PrivDir, "templates"]),
    Native = filename:nativename(Templates),
    {ok, Filenames} = file:list_dir(Native),
    lager:info("Compiling Templates..."),
    lists:foreach(fun (File) ->
                    ModuleName = filename:basename(File, ".html") ++ "_template",
                    FileAbsName = filename:join([Native, File]),
                    lager:info("  Compiling a template module named ~p~n", [ModuleName]),
                    erlydtl:compile_file(FileAbsName, list_to_atom(ModuleName), [{out_dir, false}])
                  end, Filenames),
    ok.
