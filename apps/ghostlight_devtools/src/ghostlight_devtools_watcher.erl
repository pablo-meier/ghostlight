%%% Based largely in part on the scanner in sync. Some notable changes:
%%%
%%% * It triggers calls to ghostlight_devtools.
%%% * Some 'features' are just set without configurability to keep code simple
%%%   (e.g. EnablePatching in recompiles).
%%% * Doesn't do certain fancy things like Growl:
%%%
%%% https://github.com/rustyio/sync/blob/master/src/sync_scanner.erl
-module(ghostlight_devtools_watcher).
-behaviour(gen_server).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-export([
    start_link/0,
    rescan/0,
    info/0,
    pause/0,
    unpause/0]).

-type timestamp() :: file:date_time() | 0.

-define(SERVER, ?MODULE).

-record(state, {
    modules = [] :: [module()],
    src_dirs = [] :: [file:filename()],
    src_files = [] :: [file:filename()],
    hrl_dirs = [] :: [file:filename()],
    hrl_files = [] :: [file:filename()],
    css_dirs = [] :: [file:filename()],
    css_files = [] :: [file:filename()],
    js_dirs = [] :: [file:filename()],
    js_files = [] :: [file:filename()],
    erlydtl_dirs = [] :: [file:filename()],
    erlydtl_files = [] :: [file:filename()],
    beam_lastmod = undefined :: [{module(), timestamp()}],
    src_file_lastmod = [] :: [{file:filename(), timestamp()}],
    hrl_file_lastmod = [] :: [{file:filename(), timestamp()}],
    css_file_lastmod = [] :: [{file:filename(), timestamp()}],
    js_file_lastmod = [] :: [{file:filename(), timestamp()}],
    erlydtl_file_lastmod = [] :: [{file:filename(), timestamp()}],
    timers = [],
    paused = false
}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% gen_server time!
init([]) ->
    erlang:process_flag(trap_exit, true),
    rescan(),
    lager:info("Starting the DevTools!~n"),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(compare_src_files, State) ->
    F = fun(X) ->
        LastMod = filelib:last_modified(X),
        {X, LastMod}
    end,
    NewSrcFileLastMod = lists:usort([ F(X) || X <- State#state.src_files]),

    %% Compare to previous results, if there are changes, then recompile the file...
    process_src_file_lastmod(State#state.src_file_lastmod, NewSrcFileLastMod, erl),

    %% schedule the next interval
    NewTimers = schedule_cast(compare_src_files, 1000, State#state.timers),

    %% Return with updated src_file lastmod
    NewState = State#state{ src_file_lastmod=NewSrcFileLastMod, timers=NewTimers },

    {noreply, NewState};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% Public API
rescan() ->
    lager:info("Scanning source files...~n"),
    gen_server:cast(?SERVER, discover_modules),
    gen_server:cast(?SERVER, discover_src_dirs),
    gen_server:cast(?SERVER, discover_src_files),
    gen_server:cast(?SERVER, compare_src_files),
    gen_server:cast(?SERVER, compare_beams),
    gen_server:cast(?SERVER, compare_hrl_files),
    ok.

unpause() ->
    gen_server:cast(?SERVER, unpause),
    ok.

pause() ->
    lager:info("Pausing Devtools. Call ghostlight_devtools:go() to restart~n"),
    lager:info("Pausing Sync"),
    gen_server:cast(?SERVER, pause),
    ok.

info() ->
    io:format("Sync Info...~n"),
    gen_server:cast(?SERVER, info),
    ok.



%% Internal functions
%% Note that most of this was lifted from Sync.


process_src_file_lastmod([{File, LastMod}|T1], [{File, LastMod}|T2], Type) ->
    %% Beam hasn't changed, do nothing...
    process_src_file_lastmod(T1, T2, Type);
process_src_file_lastmod([{File, _}|T1], [{File, _}|T2], Type) ->
    %% File has changed, recompile...
    recompile_src_file(File, Type),
    process_src_file_lastmod(T1, T2, Type);
process_src_file_lastmod([{File1, LastMod1}|T1], [{File2, LastMod2}|T2], Type) ->
    %% Lists are different...
    case File1 < File2 of
        true ->
            %% File was removed, do nothing...
            process_src_file_lastmod(T1, [{File2, LastMod2}|T2], Type);
        false ->
            recompile_src_file(File2, Type),
            process_src_file_lastmod([{File1, LastMod1}|T1], T2, Type)
    end;
process_src_file_lastmod([], [{File, _LastMod}|T2], Type) ->
    recompile_src_file(File, Type),
    process_src_file_lastmod([], T2, Type);
process_src_file_lastmod([], [], _) ->
    %% Done.
    ok;
process_src_file_lastmod(undefined, _Other, _) ->
    %% First load, do nothing.
    ok.

recompile_src_file(SrcFile, Type) ->
    case Type of
        erl -> ghostlight_devtools:update_erl_src(SrcFile);
        erlydtl -> ghostlight_devtools:update_erlydtl(SrcFile);
        js -> ghostlight_devtools:update_js(SrcFile);
        css -> ghostlight_devtools:update_css(SrcFile)
    end.


schedule_cast(Msg, Default, Timers) ->
    %% Cancel the old timer…
    TRef = proplists:get_value(Msg, Timers),
    timer:cancel(TRef),

    %% Lookup the interval…
    IntervalKey = list_to_atom(atom_to_list(Msg) ++ "_interval"),
    Interval = case application:get_env(ghostlight_devtools, IntervalKey) of
                   {ok, Value} -> Value;
                   _ -> Default
               end,

    %% Schedule the call…
    {ok, NewTRef} = timer:apply_after(Interval, gen_server, cast, [?SERVER, Msg]),

    %% Return the new Timers structure…
    lists:keystore(Msg, 1, Timers, {Msg, NewTRef}).


