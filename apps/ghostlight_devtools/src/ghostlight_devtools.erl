-module(ghostlight_devtools).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([trigger/0,
         update_css/1,
         update_js/1,
         update_erlydtl/1,
         update_erl_src/1]).

-define(SERVER, ?MODULE).
-define(DEVTOOLS_JAVA_NODE, 'jerry_duty@Sancho.local').

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({js, File}, State=#state{}) ->
    Dependencies = with_common_deps([ Dep || Dep <- proplists:get_value(File, js_file_mappings(), [])]),
    lager:info("Dependencies for ~p are ~p~n", [File, Dependencies]),
    Ref = make_ref(),
    {devtools_java_server, ?DEVTOOLS_JAVA_NODE} ! {self(), Ref, js, Dependencies, "/Users/pablo/projects/ghostlight/minified.js"},
    {noreply, State};

handle_cast({css, File}, State=#state{}) ->
    Dependencies = css_deps([ Dep || Dep <- proplists:get_value(File, css_file_mappings(), [])]),
    lager:info("Dependencies for ~p are ~p~n", [File, Dependencies]),
    Ref = make_ref(),
    {devtools_java_server, ?DEVTOOLS_JAVA_NODE} ! {self(), Ref, css, Dependencies, "/Users/pablo/projects/ghostlight/minified.css"},
    {noreply, State};

handle_cast({erl, File}, State=#state{}) ->
    %% Copy sync: recompile, reload.
    case compile:file(File, [verbose, report_errors, report_warnings, binary]) of
        {ok, ModuleName, Binary} ->
            Result = code:load_binary(ModuleName, File, Binary),
            lager:info("Compiled/Loaded file ~p, return on load is ~p~n", [Result]);
        {error, Errors, Warnings} ->
            lager:error("Error compiling ~p: ~p~nWarnings:~p~n", [File, Errors, Warnings])
    end,
    {noreply, State};

handle_cast({erlydtl, _File}, State=#state{}) ->
    %% build new template, reload.
    {noreply, State}.

handle_info(Info, State) ->
    lager:info("Other info received! ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

trigger() ->
    update_js("insert_work_form.js"),
    update_css("insert_show.html").

%% Ideally this is done in a config file somewhere, but this is a start. A proplist with
%% 3 values:
%% * The filename to check on an edit.
%% * The list of dependencies that file has (what to compile first).
%% * The output 'basename.' (in debug, we'll point templates to the basename. In prod, we'll
%%   append a version number or something so we invalidate caches on change.
%%
%% Note that all JS in this system will require jQuery, Foundation, then a call to foundation.start().
js_file_mappings() ->
    [{"insert_work_form.js", [lodash]},
     {"view_show.js", [d3, 'cal-heatmap', moment, 'moment-range']}].

prepare_dependency(Dep) ->
    filename_from_privdir(Dep, "js", fun registered_dep/1).

registered_dep(lodash) -> "lodash.min.js";
registered_dep(d3) -> "d3.v3.min.js";
registered_dep('cal-heatmap') -> "cal-heatmap.min.js";
registered_dep(moment) -> "moment.min.js";
registered_dep(jquery) -> "jquery.js";
registered_dep(foundation) -> "foundation.min.js";
registered_dep(foundation_start) -> "foundation_start.js".

with_common_deps(Deps) ->
    [ prepare_dependency(Dep) || Dep <- lists:append([ 
                                           [jquery,
                                            foundation],
                                           Deps,
                                           [foundation_start] ]) ].

css_file_mappings() ->
    [{"show.html", ['cal-heatmap']},
     {"insert_show.html", [pickadate, pickadate_date, pickadate_time]}].

registered_css('cal-heatmap') -> "cal-heatmap.css";
registered_css(pickadate) -> "pickadate.css";
registered_css(pickadate_date) -> "pickadate.date.css";
registered_css(pickadate_time) -> "pickadate.time.css";
registered_css(foundation) -> "foundation.css".

prepare_css(Dep) ->
    filename_from_privdir(Dep, "css", fun registered_css/1).

css_deps(Deps) ->
    [ prepare_css(Dep) || Dep <- lists:append([ [foundation], Deps ]) ].


filename_from_privdir(Dep, FileType, Lookup) ->
    PrivDir = code:priv_dir(ghostlight),
    Templates = filename:join([PrivDir, "static", FileType, "vendor", Lookup(Dep)]),
    filename:nativename(Templates).


update_js(Filename) ->
    gen_server:cast(?MODULE, {js, Filename}).

update_css(Filename) ->
    gen_server:cast(?MODULE, {css, Filename}).

update_erl_src(Filename) ->
    gen_server:cast(?MODULE, {erl, Filename}).

update_erlydtl(Filename) ->
    gen_server:cast(?MODULE, {erlydtl, Filename}).

