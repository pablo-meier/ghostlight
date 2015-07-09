%%%-------------------------------------------------------------------
%%% @author pablo
%%% @copyright (C) 2015, pablo
%%% @doc
%%% While not strictly 'static' (Cowboy has great resources for that),
%%% this is for pages that only require rendering, and don't change over
%%% the lifetime of the site — The homepage, the FAQ, the About page,
%%% etc. We'd still like to render them as ErlyDTL templates, but don't
%%% want to re-render them every time.
%%% @end
%%% Created : 2015-07-08 08:52:55.071157
%%%-------------------------------------------------------------------
-module(ghostlight_static).

-behaviour(gen_server).

%% API
-export([start_link/0,
         init/2,
         terminate/3]).


%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(static_spec, { source :: binary(), title :: binary(), path :: binary() }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    Pages = [#static_spec{ source = <<"FAQ.md">>, title = <<"FAQ!">>, path = <<"/faq.html">> },
             #static_spec{ source = <<"About.md">>, title = <<"About Ghostlight">>, path = <<"/about.html">>},
             #static_spec{ source = <<"homepage.html">>, title = <<"Ghostlight!">>, path = <<"/index.html">>},
             #static_spec{ source = <<"homepage.html">>, title = <<"Ghostlight!">>, path = <<"/">>}],
    State = lists:foldl(fun (Record, Accum) ->
                            add_to_map(Record, Accum)
                        end, maps:new(), Pages),
    {ok, State}.

handle_call({page, Page}, _From, State) ->
    Spec = maps:get(Page, State),
    Reply = render_it(Spec),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

add_to_map(Spec=#static_spec{source=_Source, title=_Title, path=Path}, Map) ->
%    PrivDir = code:priv_dir(ghostlight),
%    Templates = filename:join([PrivDir, "unchanging", Source]),
%    Native = filename:nativename(Templates),
%    {ok, Content} = file:read_file(Native),
%    lager:info("For ~p we have ~p~n", [Path, Content]),
%    Markdowned = ghostlight_markdown:parse_markdown(Content),
%    {ok, Data} = unchanging_template:render([{title, Title}, {content, Markdowned}]),
%    maps:put(Path, Data, Map).
    maps:put(Path, Spec, Map).

render_it(#static_spec{source=Source, title=Title, path=_Path}) ->
    PrivDir = code:priv_dir(ghostlight),
    Templates = filename:join([PrivDir, "unchanging", Source]),
    Native = filename:nativename(Templates),
    Rendered = fetch_rendered_content(re:run(Source, "^[a-zA-Z0-9_]+\.md$"), Native),
    {ok, Data} = unchanging_template:render([{title, Title}, {content, Rendered}]),
    Data.

fetch_rendered_content({match, _}, Native) ->
    {ok, Content} = file:read_file(Native),
    Markdowned = ghostlight_markdown:parse_markdown(Content),
    Markdowned;

fetch_rendered_content(nomatch, Native) ->
    {ok, Content} = file:read_file(Native),
    Content.
 


%%%===================================================================
%%% External API
%%%===================================================================

init(Req, _Opts) ->
    ensure_method(Req, cowboy_req:method(Req)).

%%% @doc Proceeds with the request if it's a get. Else, we return 405
%%% Method not allowed.
ensure_method(Req, <<"GET">>) ->
    Req2 = cowboy_req:set_meta(response_type, html, Req),
    Response = get_content(cowboy_req:path(Req2)),
    Req3 = cowboy_req:reply(200,
                            [{<<"content-type">>, <<"text/html; charset=utf-8">>}],
                            Response,
                            Req2),
    {ok, Req3, []};

ensure_method(Req, _) ->
    Req2 = cowboy_req:reply(405, Req),
    {ok, Req2, []}.


get_content(Page) ->
    gen_server:call(?SERVER, {page, Page}).


%% Provided for cowboy, but I have no intention of properly using it.
terminate(_Reason, _Req, _State) ->
  ok.
