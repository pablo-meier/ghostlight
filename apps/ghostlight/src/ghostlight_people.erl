-module(ghostlight_people).
-export([init/2,
         content_types_provided/2,
         content_types_accepted/2,
         allowed_methods/2]).
-export([person_to_html/2,
         person_to_json/2]).

-export([json_to_record/1]).

-include("apps/ghostlight/include/ghostlight_data.hrl").

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"DELETE">>],
     Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"text/html">>, person_to_html},
      {<<"application/json">>, person_to_json}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"application/json">>, post_json}
     ], Req, State}.

person_to_html(Req, State) ->
    PersonId = cowboy_req:binding(person_id, Req),
    case PersonId of
        <<"new">> ->
            {ok, Body} = insert_person_template:render([]),
            {Body, Req, State};
        _ ->
            PersonRecord = ghostlight_db:get_person(PersonId),
            lager:info("PersonRecord returned from DB is ~p~n", [PersonRecord]),
            ForTemplate = [{name, <<"pablo">>}],
            {ok, Body} = person_template:render(ForTemplate),
            {Body, Req, State}
    end.


json_to_record({Person}) ->
    case proplists:get_value(<<"id">>, Person) of
        undefined ->
            {name, proplists:get_value(<<"name">>, Person)};
        Id ->
            {id, Id}
    end.

person_to_json(Req, State) ->
    {<<"{ \"status\": \"ok\" }">>, Req, State}.

