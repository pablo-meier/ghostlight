%%%-------------------------------------------------------------------
%%% @author pablo
%%% @copyright (C) 2015, pablo
%%% @doc
%%% HTML-only endpoint (for now?) for a user to see all their search
%%% results get hints back for what is there. Query parameters are
%%% (uncharacteristically) passed in via querystring, so they are
%%% copy-pastable URLs for others to see. As with the REST endpoints,
%%% 'Accepts' header determines whether we return JSON or HTML.
%%%
%%% Search parameters are the following:
%%%   q = <string>
%%%     The search string to look for.
%%%
%%%  === Later!
%%%
%%%   exact = true | false
%%%     Is the search fuzzy, or exact? Default `false`
%%%
%%%   includes = works | shows | organizations | people
%%%     Comma-separated list of which resources to search
%%%     for. Defaults to all (`works,shows,organizations,people`)
%%%
%%% @end
%%% Created : 2015-06-23 00:09:38.668156
%%%-------------------------------------------------------------------
-module(ghostlight_search).

-export([init/2,
         terminate/3]).

-record(ghost_params, {
    q = <<"">>     :: binary(),
    exact = false  :: true | false,

    includes = default_includes() :: list(works | shows | organizations | people)
}).

init(Req, _Opts) ->
    Req2 = cowboy_req:set_meta(response_type, html, Req),
    Params = get_params(cowboy_req:parse_qs(Req2)),
    Response = gather_content(Params),

    ReturnType = cowboy_req:header(<<"accept">>, Req2, <<"text/html">>),
    deliver(ReturnType, Response, Req2).


%%% @doc Determines what content to return to the requester — Make no request
%%% in the case of an empty request (i.e. user going to ghostlight.io/search)
%%% but otherwise return a list of resources.
gather_content(#ghost_params{q = <<"">>}) ->
    [];
gather_content(#ghost_params{q=Query, exact=_Exact, includes=_Includes}) ->
    lager:info("Query is ~p~n", [Query]),
    Response = ghostlight_fulltext:find(Query),
    lager:info("Response is ~p~n", [Response]),
    Response.


%%% @doc Takes and formats the data to return appropriately to the requester,
%%% per their expressed preference.
deliver(<<"application/json">>, Response, Req) ->
    Req2 = cowboy_req:reply(200,
                            [{<<"content-type">>, <<"application/json; charset=utf-8">>}],
                            jsx:encode(Response),
                            Req),
    {ok, Req2, []};

%% Default to HTML
deliver(_, Response, Req) ->
    Proplist = [{results, [ proplist_of(SearchResult) || SearchResult <- Response ]}],
    lager:info("~n~n~n~n~p~n~n~n", [Proplist]),
    {ok, Body} = search_results_template:render(Proplist),
    Req2 = cowboy_req:reply(200,
                            [{<<"content-type">>, <<"text/html; charset=utf-8">>}],
                            Body,
                            Req),
    {ok, Req2, []}.


proplist_of({Type=show, Show}) ->
    [{<<"type">>, Type},
     {<<"data">>, ghostlight_show:record_to_proplist(Show)}];
proplist_of({Type=work, Work}) ->
    [{<<"type">>, Type},
     {<<"data">>, ghostlight_work:record_to_proplist(Work)}];
proplist_of({Type=org, Org}) ->
    [{<<"type">>, Type},
     {<<"data">>, ghostlight_org:record_to_proplist(Org)}];
proplist_of({Type=person, Person}) ->
    [{<<"type">>, Type},
     {<<"data">>, ghostlight_people:record_to_proplist(Person)}].

get_params(Params) ->
    Query = proplists:get_value(<<"q">>, Params, <<"">>), 
    Exact = parse_exact(proplists:get_value(<<"exact">>, Params)), 
    Includes = parse_includes(proplists:get_value(<<"includes">>, Params)), 
    #ghost_params{
        q = Query,
        exact = Exact,
        includes = Includes
    }.


parse_exact(<<"true">>) -> true;
parse_exact(_) -> false.


parse_includes(Bin) when is_binary(Bin) and byte_size(Bin) < 50 ->
    Includes = binary:split(Bin, <<",">>),
    case lists:all(fun is_viable_include/1, Includes) of
        true -> [ list_to_atom(binary_to_list(X)) || X <- Includes ];
        false -> default_includes()
    end;
parse_includes(_) -> default_includes().


is_viable_include(<<"works">>) -> true;
is_viable_include(<<"shows">>) -> true;
is_viable_include(<<"organizations">>) -> true;
is_viable_include(<<"people">>) -> true;
is_viable_include(_) -> false.

default_includes() -> [works, shows, organizations, people].

%% Provided for cowboy, but I have no intention of properly using it.
terminate(_Reason, _Req, _State) ->
  ok.
