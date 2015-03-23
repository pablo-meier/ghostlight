-module(ghostlight_show).
-export([init/2]).
-export([content_types_provided/2,
         content_types_accepted/2,
         allowed_methods/2]).
-export([show_to_html/2,
         show_to_json/2,
        
         post_json/2]).

-include("apps/ghostlight/include/ghostlight_data.hrl").


init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"DELETE">>],
     Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"text/html">>, show_to_html},
      {<<"application/json">>, show_to_json}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"application/json">>, post_json}
     ], Req, State}.


show_to_html(Req, State) ->
    ShowId = cowboy_req:binding(show_id, Req),
    case ShowId of
        <<"new">> ->
            {ok, Body} = insert_show_template:render([]),
            {Body, Req, State};
        _ ->
            ShowRecord = ghostlight_db:get_show(ShowId),
            ForTemplate = record_to_proplist(ShowRecord),
            {ok, Body} = show_template:render(ForTemplate),
            {Body, Req, State}
    end.

record_to_proplist(#show{
                     title=Title,
                     org=#organization{
                            id=OrgId,
                            name=OrgName
                         },
                     special_thanks=SpecialThanks,
                     performances=Performances,
                     dates=Dates}) ->
    [{title, Title},
     {org, [{org_id, OrgId}, {org_name, OrgName}]},
     {special_thanks, SpecialThanks},
     {dates, Dates},
     {performances, performances_to_proplists(Performances)}
    ].


performances_to_proplists(Performances) ->
    [ [{work, [{title, WorkTitle},
               {work_id, WorkId},
               {authors, personlist_as_proplist(WorkAuthors)}]},
       {directors, personlist_as_proplist(Directors)},
       {onstage, onstage_as_proplists(Onstage)},
       {offstage, offstage_as_proplists(Offstage)}] || #performance{ 
                                                           work=#work{ id = WorkId, title = WorkTitle, authors = WorkAuthors},
                                                           onstage=Onstage,
                                                           offstage=Offstage,
                                                           directors=Directors} <- Performances ].

onstage_as_proplists(OnstageList) ->
    [ [{name, Name},
       {role, Role},
       {person_id, PersonId}] || #onstage{ person=#person{id = PersonId, name = Name}, role = Role} <- OnstageList].
offstage_as_proplists(OnstageList) ->
    [ [{name, Name},
       {job, Job},
       {person_id, PersonId}] || #offstage{ person=#person{id = PersonId, name = Name}, job = Job} <- OnstageList].
personlist_as_proplist(DirectorList) ->
    [ [{name, Name},
       {person_id, PersonId}] || #person{id = PersonId, name = Name} <- DirectorList].

show_to_json(Req, State) ->
    ShowId = cowboy_req:binding(show_id, Req),
    _ShowRecord = ghostlight_db:get_show(ShowId),
%    Body = jiffy:encode({CorrectedPropList}),
    {<<"{ \"status\": \"ok\" }">>, Req, State}.


post_json(Req, State) ->
    % upload the body, return yes or no
    {ok, RequestBody, Req2} = cowboy_req:body(Req),
    ShowRecord = show_json_to_record(RequestBody),
    lager:info("~nShowRecord is ~p", [ShowRecord]),
    Response = ghostlight_db:insert_show(ShowRecord),
    lager:info("~nResponse from DB server is ~p~n", [Response]),

    {true, cowboy_req:set_resp_body(<<"ok">>, Req2), State}.


show_json_to_record(JsonInput) ->
    {Decoded} = jiffy:decode(JsonInput),
    lager:info("~nWe have an Erlang object: ~p~n", [{Decoded}]),

    Title = proplists:get_value(<<"title">>, Decoded),
    SpecialThanks = proplists:get_value(<<"special_thanks">>, Decoded),
    Dates = lists:map(fun iso8601:parse/1, proplists:get_value(<<"dates">>, Decoded)),
    Org = organization_json_to_record(proplists:get_value(<<"org">>, Decoded)),
    Performances = lists:map(fun performance_json_to_record/1, proplists:get_value(<<"performances">>, Decoded)),

    #show{
        title = Title,
        special_thanks = SpecialThanks,
        dates = Dates,
        org = Org,
        performances = Performances
    }.


performance_json_to_record({Proplist}) ->
    Work = work_json_to_record(proplists:get_value(<<"work">>, Proplist)),
    Onstage = lists:map(fun onstage_json_to_record/1, proplists:get_value(<<"onstage">>, Proplist, [])),
    Offstage = lists:map(fun offstage_json_to_record/1, proplists:get_value(<<"offstage">>, Proplist, [])),
    Directors = lists:map(fun ghostlight_people:json_to_record/1, proplists:get_value(<<"directors">>, Proplist)),
    #performance {
       work = Work,
       directors = Directors,
       onstage = Onstage,
       offstage = Offstage
    }.


work_json_to_record({Proplist}) ->
    #work {
       title = proplists:get_value(<<"title">>, Proplist),
       authors = lists:map(fun ghostlight_people:json_to_record/1, proplists:get_value(<<"authors">>, Proplist))
    }.


organization_json_to_record({Org}) ->
    Name = proplists:get_value(<<"name">>, Org),
    Tagline = proplists:get_value(<<"tagline">>, Org),
    Description = proplists:get_value(<<"description">>, Org),
    #organization{
       name = Name,
       tagline = Tagline,
       description = Description
    }.


onstage_json_to_record({Onstage}) ->
    Performer = ghostlight_people:json_to_record(proplists:get_value(<<"performer">>, Onstage)),
    Role = proplists:get_value(<<"role">>, Onstage),
    #onstage{
      person = Performer,
      role = Role
    }.

offstage_json_to_record({Offstage}) ->
    Contributor = ghostlight_people:json_to_record(proplists:get_value(<<"contributor">>, Offstage)),
    Job = proplists:get_value(<<"job">>, Offstage),
    #offstage{
      person = Contributor,
      job = Job 
    }.

