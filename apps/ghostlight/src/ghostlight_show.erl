-module(ghostlight_show).
-export([init/2]).
-export([content_types_provided/2,
         content_types_accepted/2,
         charsets_provided/2,
         allowed_methods/2]).
-export([show_to_html/2,
         show_to_json/2,
         post_json/2,
         record_to_json/1]).

-include("apps/ghostlight/include/ghostlight_data.hrl").

%% HTML
%%
%% GET /id         ------------- DONE
%% GET /           ------------- DONE
%% GET /new
%% GET /id/delete
%% GET /id/edit
%%
%% JSON
%%
%% GET /id         ------------- DONE
%% GET /           ------------- DONE
%% POST /          ------------- DONE
%% PUT /id
%% DELETE /id

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>],
     Req, State}.
charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.
content_types_provided(Req, State) ->
    {[
      {<<"text/html">>, show_to_html},
      {<<"application/json">>, show_to_json}
     ], Req, State}.
content_types_accepted(Req, State) ->
    {[
      {<<"application/json; charset=utf-8">>, post_json},
      {<<"application/json">>, post_json}
     ], Req, State}.


show_to_html(Req, State) ->
    ShowId = cowboy_req:binding(show_id, Req),
    Command = cowboy_req:binding(command, Req),
    case {ShowId, Command} of
        {undefined, undefined} ->
            ShowList = ghostlight_db:get_show_listings(),
            ForTemplate = [{shows, [ record_to_proplist(Show) || Show <- ShowList ]}],
            {ok, Body} = show_listing_template:render(ForTemplate),
            {Body, Req, State};
        {<<"new">>, undefined} ->
            {ok, Body} = insert_show_template:render([]),
            {Body, Req, State};
        {_, undefined} ->
            ShowRecord = ghostlight_db:get_show(ShowId),
            ForTemplate = record_to_proplist(ShowRecord),
            {ok, Body} = show_template:render(ForTemplate),
            {Body, Req, State};
        {_, <<"edit">>} ->
            ShowRecord = ghostlight_db:get_show(ShowId, markdown),
            AsJson = jiffy:encode(record_to_json(ShowRecord)),
            {ok, Body} = insert_show_template:render([{name, ShowRecord#show.title},
                                                      {editmode, AsJson}]),
            {Body, Req, State}
    end.

show_to_json(Req, State) ->
    ShowId = cowboy_req:binding(show_id, Req),
    case ShowId of
        undefined ->
            ShowList = ghostlight_db:get_show_listings(),
            ToEncode = {[{<<"shows">>, [ record_to_json(Show) || Show <- ShowList ]}]},
            Body = jiffy:encode(ToEncode),
            {Body, Req, State};
        _ ->
            ShowRecord = ghostlight_db:get_show(ShowId),
            AsJson = jiffy:encode(record_to_json(ShowRecord)),
            {AsJson, Req, State}
    end.


%% HTML

record_to_proplist(#show{
                     id=ShowId,
                     title=Title,
                     producers=Producers,
                     special_thanks=SpecialThanks,
                     description=Description,
                     hosts=Hosts,
                     press_links=PressLinks,
                     external_links=ExternalLinks,
                     performances=Performances,
                     dates=Dates}) ->
    [{id, ShowId},
     {title, Title},
     {producers, [ producer_to_proplist(Producer) || Producer <- Producers ]},
     {special_thanks, SpecialThanks},
     {dates, Dates},
     {hosts, [ [{<<"host_id">>, HostId}, {<<"host_name">>, HostName}] || #person{id=HostId, name=HostName} <- Hosts]},
     {press, [ [{<<"link">>, Url}, {<<"description">>, LinkDesc}] || #press_link{link=Url, description=LinkDesc} <- PressLinks]},
     {description, ghostlight_utils:remove_null(Description)},
     {links, ghostlight_utils:external_links_record_to_proplist(ExternalLinks)},
     {performances, [ performance_to_proplists(Performance) || Performance <- Performances ] }
    ].

producer_to_proplist(#organization{id=OrgId, name=OrgName}) ->
    [{producer_id, OrgId},
     {producer_name, OrgName},
     {is_org, true}];
producer_to_proplist(#person{id=PersonId, name=Name}) ->
    [{producer_id, PersonId},
     {producer_name, Name},
     {is_org, false}].

performance_to_proplists(#performance{ 
                             work=#work{
                                 id = WorkId,
                                 title = WorkTitle,
                                 authors = WorkAuthors
                             },
                             onstage=Onstage,
                             offstage=Offstage,
                             directors=Directors,
                             directors_note=DirectorsNote,
                             description=Description}) ->
    [{work, [{title, WorkTitle},
             {work_id, WorkId},
             {authors, personlist_as_proplist(WorkAuthors)}]},
     {directors_note, ghostlight_utils:remove_null(DirectorsNote)},
     {description, ghostlight_utils:remove_null(Description)},
     {directors, personlist_as_proplist(Directors)},
     {onstage, onstage_as_proplists(Onstage)},
     {offstage, offstage_as_proplists(Offstage)}].

onstage_as_proplists(OnstageList) ->
    [ [{name, Name},
       {role, ghostlight_utils:remove_null(Role)},
       {person_id, PersonId}] || #onstage{ person=#person{id = PersonId, name = Name}, role = Role} <- OnstageList].
offstage_as_proplists(OnstageList) ->
    [ [{name, Name},
       {job, Job},
       {person_id, PersonId}] || #offstage{ contributor=#person{id = PersonId, name = Name}, job = Job} <- OnstageList].
personlist_as_proplist(DirectorList) ->
    [ [{name, Name},
       {person_id, PersonId}] || #person{id = PersonId, name = Name} <- DirectorList].

record_to_json(#show{
                  id=ShowId,
                  title=ShowTitle,
                  performances=Performances,
                  special_thanks=SpecialThanks,
                  dates=Dates}) ->
    ghostlight_utils:json_with_valid_values([
        {<<"id">>, ShowId},
        {<<"show_title">>, ShowTitle},
        {<<"special_thanks">>, SpecialThanks},
        {<<"performances">>, [ performance_record_to_json(Performance) || Performance <- Performances ]},
        {<<"dates">>, [ ghostlight_utils:erl_date_to_iso8601(Date) || Date <- Dates ]}
    ]).
performance_record_to_json(#performance{
                             work=Work,
                             directors=Directors,
                             onstage=Onstage,
                             offstage=Offstage}) ->
    ghostlight_utils:json_with_valid_values([
        {<<"work">>, ghostlight_work:record_to_json(Work)},
        {<<"directors">>, [ ghostlight_people:record_to_json(Director) || Director <- Directors ]},
        {<<"onstage">>, [ onstage_as_json(Performer) || Performer <- Onstage ]},
        {<<"offstage">>, [ offstage_as_json(Contributor) || Contributor <- Offstage ]}
    ]).
onstage_as_json(#onstage{
                   role=Role,
                   person=Person
                }) ->
    ghostlight_utils:json_with_valid_values([
        {<<"role">>, Role},
        {<<"person">>, ghostlight_people:record_to_json(Person)}
    ]).
offstage_as_json(#offstage{
                   job=Job,
                   contributor=Person
                }) ->
    ghostlight_utils:json_with_valid_values([
        {<<"job">>, Job},
        {<<"contributor">>, ghostlight_people:record_to_json(Person)}
    ]).


post_json(Req, State) ->
    % upload the body, return yes or no
    {ok, RequestBody, Req2} = cowboy_req:body(Req),
    {Decoded} = jiffy:decode(RequestBody),
    ShowRecord = show_json_to_record(Decoded),
    Method = cowboy_req:method(Req2),
    case {ShowRecord#show.id, Method} of
        {null, <<"POST">>} ->
            ShowId = ghostlight_db:insert_show(ShowRecord),
            Response = jiffy:encode({[{<<"status">>, <<"ok">>}, {<<"id">>, list_to_binary(ShowId)}]}),
            {true, cowboy_req:set_resp_body(Response, Req2), State};
        {_Else, <<"POST">>} ->
            Body = jiffy:encode({[{<<"error">>, <<"You may not insert a person with the field 'id'.">>}]}),
            Req3 = cowboy_req:set_resp_body(Body, Req2),
            {false, Req3, State};
        {null, <<"PUT">>} ->
            Body = jiffy:encode({[{<<"error">>, <<"You must PUT on an existing resource.">>}]}),
            Req3 = cowboy_req:set_resp_body(Body, Req2),
            {false, Req3, State};
        {_PersonId, <<"PUT">>} ->
            Success = ghostlight_db:update_show(ShowRecord),
            case Success of
                true ->
                    Response = jiffy:encode({[{<<"status">>, ok}]}),
                    {true, cowboy_req:set_resp_body(Response, Req2), State};
                false ->
                    Body = jiffy:encode({[{<<"error">>, <<"An error occurred.">>}]}),
                    Req3 = cowboy_req:set_resp_body(Body, Req2),
                    {false, Req3, State}
            end
    end.


show_json_to_record(Decoded) ->
    Title = proplists:get_value(<<"title">>, Decoded),
    SpecialThanks = proplists:get_value(<<"special_thanks">>, Decoded),
    Dates = lists:map(fun iso8601:parse/1, proplists:get_value(<<"dates">>, Decoded)),
    Producers = [ ghostlight_utils:person_or_org_json_to_record(Producer)
                  || Producer <- proplists:get_value(<<"producers">>, Decoded)],
    Performances = lists:map(fun performance_json_to_record/1, proplists:get_value(<<"performances">>, Decoded)),

    {LinksObj} = proplists:get_value(<<"social">>, Decoded, {[]}),
    ExternalLinks = ghostlight_utils:external_links_json_to_record(LinksObj),
    Hosts = [ ghostlight_people:json_to_record(Host) || Host <- proplists:get_value(<<"hosts">>, Decoded, []) ],

    Press = proplists:get_value(<<"press">>, Decoded, []),
    PressLinks = [ #press_link{link=proplists:get_value(<<"link">>, Link, null),
                               description=proplists:get_value(<<"description">>, Link, null)} || {Link} <- Press],

    #show{
        title = Title,
        special_thanks = SpecialThanks,
        dates = Dates,
        producers = Producers,
        hosts = Hosts,
        performances = Performances,
        external_links=ExternalLinks,
        press_links=PressLinks
    }.

performance_json_to_record({Proplist}) ->
    Work = ghostlight_work:json_to_record(proplists:get_value(<<"work">>, Proplist)),
    Onstage = lists:map(fun onstage_json_to_record/1, proplists:get_value(<<"onstage">>, Proplist, [])),
    Offstage = [ offstage_json_to_record(Offstage) || Offstage <- proplists:get_value(<<"offstage">>, Proplist, []) ],
    Directors = lists:map(fun ghostlight_people:json_to_record/1, proplists:get_value(<<"directors">>, Proplist, [])),
    DirectorNote = proplists:get_value(<<"directors_note">>, Proplist, null),
    Description = proplists:get_value(<<"description">>, Proplist, null),
    #performance {
       work = Work,
       directors = Directors,
       onstage = Onstage,
       offstage = Offstage,
       directors_note = DirectorNote,
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
    Contributor = ghostlight_utils:person_or_org_json_to_record(proplists:get_value(<<"contributor">>, Offstage)),
    Job = proplists:get_value(<<"job">>, Offstage),
    #offstage{
      contributor = Contributor,
      job = Job 
    }.

