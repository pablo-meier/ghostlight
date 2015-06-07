-module(ghostlight_people).
-export([init/2,
         content_types_provided/2,
         content_types_accepted/2,
         charsets_provided/2,
         allowed_methods/2]).

-export([person_to_html/2,
         person_to_json/2]).

-export([json_to_record/1,
         record_to_json/1,
         record_to_proplist/1,
        
         post_json/2]).

-include("apps/ghostlight/include/ghostlight_data.hrl").

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>],
     Req, State}.
charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.
content_types_provided(Req, State) ->
    {[
      {<<"text/html">>, person_to_html},
      {<<"application/json">>, person_to_json}
     ], Req, State}.
content_types_accepted(Req, State) ->
    {[
      {<<"application/json; charset=utf-8">>, post_json},
      {<<"application/json">>, post_json}
     ], Req, State}.

person_to_html(Req, State) ->
    PersonId = cowboy_req:binding(person_id, Req),
    Command = cowboy_req:binding(command, Req),
    case {PersonId, Command} of
        {undefined, undefined} ->
            PersonList = ghostlight_db:get_person_listings(),
            ForTemplate = [{people, [ record_to_proplist(Person) || Person <- PersonList ]}],
            {ok, Body} = person_listing_template:render(ForTemplate),
            {Body, Req, State};
        {<<"new">>, _} ->
            {ok, Body} = insert_person_template:render([]),
            {Body, Req, State};
        {_, undefined} ->
            PersonRecord = ghostlight_db:get_person(PersonId),
            ForTemplate = record_to_proplist(PersonRecord),
            {ok, Body} = person_template:render(ForTemplate),
            {Body, Req, State};
        {_, <<"edit">>} ->
            PersonRecord = ghostlight_db:get_person(PersonId, markdown),
            AsJson = jiffy:encode(record_to_json(PersonRecord)),
            {ok, Body} = insert_person_template:render([{name, PersonRecord#person_return.person#person.name},
                                                        {editmode, AsJson}]),
            {Body, Req, State}
    end.

%% Makes the Record returned from the DB into a proplist we can feed the template.
%% Aw hell yeah Pattern Matching.
record_to_proplist(#person_return{
                     person=#person{
                        id=PersonId,
                        name=Name,
                        description=Description,
                        external_links=ExternalLinks
                     },
                     shows_produced=Produced,
                     authored=Authored,
                     directed=Directed,
                     onstage=Onstage,
                     offstage=Offstage,
                     orgs_employee=Orgs}) ->

    OnstageProplist = [ make_onstage_proplist(OnstageShow) || OnstageShow <- Onstage ],
    ProducedProplist = [ make_producer_proplist(ProducedShow) || ProducedShow <- Produced ],
    OffstageProplist = [ [{show_id, ShowId}, 
                          {show_title, ShowTitle},
%                          {org_id, OrgId},
%                          {org_name, OrgName},
                          {work_id, WorkId},
                          {work_title, WorkTitle},
                          {job, Job}] || #show{ title=ShowTitle,
                                                id=ShowId,
%                                                org=#organization{id=OrgId, name=OrgName},
                                                performances=[#performance{
                                                                work=#work{ id=WorkId, title=WorkTitle },
                                                                offstage=[#offstage{ job=Job }]
                                                             }]
                                              } <- Offstage],
    AuthorshipProplist = [ [{work_id, WorkId},
                            {title, Title}] || #work{id=WorkId, title=Title} <- Authored],

    DirectorProplist = [[{show_id, ShowId},
                         {show_title, ShowTitle},
%                         {org_id, OrgId},
%                         {org_name, OrgName},
                         {work_id, WorkId},
                         {work_title, WorkTitle}] || #show{ title=ShowTitle,
                                                            id=ShowId,
                                                            % org=#organization{id=OrgId, name=OrgName},
                                                            performances=[#performance{
                                                                            work=#work{ id=WorkId, title=WorkTitle }
                                                                         }]} <- Directed ],

    OrgProplist = [ [{org_id, OrgId},
                     {org_name, OrgName},
                     {position, Position}] || #org_work{org_id=OrgId, org_name=OrgName, title=Position} <- Orgs ],

    [{id, PersonId},
     {name, Name},
     {description, Description},
     {onstage_list, OnstageProplist},
     {offstage_list, OffstageProplist},
     {authorship, AuthorshipProplist},
     {director, DirectorProplist},
     {producer, ProducedProplist},
     {organizations, OrgProplist},
     {links, ghostlight_utils:external_links_record_to_proplist(ExternalLinks)}];

record_to_proplist(#person{
                      id=PersonId,
                      name=PersonName
                   }) ->
    [{person_name, PersonName},
     {person_id, PersonId}].


make_onstage_proplist( #show{ title=ShowTitle,
                              id=ShowId,
                              producers=Producers,
                              performances=[#performance{
                                                work=#work{ id=WorkId, title=WorkTitle },
                                                onstage=[#onstage{ role=Role }]
                                            }]}) ->
    [{show_id, ShowId},
     {show_title, ShowTitle},
     {producers, [ person_or_org_to_proplist(Producer) || Producer <- Producers ]},
     {work_id, WorkId},
     {work_title, WorkTitle},
     {role, ghostlight_utils:remove_null(Role)}].

make_producer_proplist(#show{
                          id=ShowId,
                          title=ShowTitle,
                          performances=Performances}) ->
    [{show_id, ShowId},
     {show_title, ShowTitle},
     {performances, [ work_proplist_from_performance(Performance) || Performance <- Performances ]} ].

work_proplist_from_performance(#performance {
                                    work=#work{
                                            id=WorkId,
                                            title=WorkTitle
                                           }
                                 }) ->
    [{work_id, WorkId}, {work_title, WorkTitle}].


person_or_org_to_proplist(#person{
                                id=PersonId,
                                name=PersonName
                            }) ->
    [{is_org, false}, {id, PersonId}, {name, PersonName}];
person_or_org_to_proplist(#organization {
                                id=OrgId,
                                name=OrgName
                            }) ->
    [{is_org, true}, {id, OrgId}, {name, OrgName}].


json_to_record({Person}) ->
    PersonId = proplists:get_value(<<"id">>, Person, null),
    PersonName = proplists:get_value(<<"name">>, Person, null),
    PersonDescription = proplists:get_value(<<"description">>, Person, null),
    ExternalLinks = ghostlight_utils:external_links_json_to_record(Person),

    #person{
       id = PersonId,
       name = PersonName,
       description = PersonDescription,
       external_links=ExternalLinks
    }.

person_to_json(Req, State) ->
    PersonId = cowboy_req:binding(person_id, Req),
    case PersonId of
        undefined ->
            PersonList = ghostlight_db:get_person_listings(),
            ToEncode = {[{<<"people">>, [ record_to_json(Person) || Person <- PersonList ]}]},
            Body = jiffy:encode(ToEncode),
            {Body, Req, State};
        _ ->
            PersonRecord = ghostlight_db:get_person(PersonId),
            AsJson = jiffy:encode(record_to_json(PersonRecord)),
            {AsJson, Req, State}
    end.


record_to_json(Person=#person{}) ->
    Proplist = record_to_json_shared(Person),
    ghostlight_utils:json_with_valid_values(Proplist);
record_to_json(#person_return{
                     person=Person,
                     authored=Authored,
                     directed=Directed,
                     onstage=Onstage,
                     offstage=Offstage,
                     orgs_employee=Orgs
               }) ->
    PersonArr = record_to_json_shared(Person),
    ghostlight_utils:json_with_valid_values(lists:append([
        PersonArr,
        [{<<"authored">>, [ ghostlight_work:record_to_json(Work) || Work <- Authored ]},
         {<<"directed">>, [ ghostlight_work:record_to_json(Work) || Work <- Directed ]},
         {<<"onstage">>, [ ghostlight_show:record_to_json(Show) || Show <- Onstage ]},
         {<<"offstage">>, [ ghostlight_show:record_to_json(Show) || Show <- Offstage ]},
         {<<"organizations">>, [ ghostlight_org:record_to_json(Org) || Org <- Orgs ]}]
    ])).


record_to_json_shared(#person{
                          id=PersonId,
                          name=PersonName,
                          description=Description,
                          external_links=Links}) ->
    [
     {<<"person_id">>, PersonId},
     {<<"name">>, PersonName},
     {<<"description">>, Description},
     {<<"social">>, ghostlight_utils:external_links_record_to_json(Links)}
    ].


post_json(Req, State) ->
    {ok, RequestBody, Req2} = cowboy_req:body(Req),
    AsJson = jiffy:decode(RequestBody),
    PersonRecord = json_to_record(AsJson),
    Method = cowboy_req:method(Req2),
    case {PersonRecord#person.id, Method} of
        {null, <<"POST">>} ->
            PersonId = ghostlight_db:insert_person(PersonRecord),
            Response = jiffy:encode({[{<<"status">>, <<"ok">>}, {<<"id">>, list_to_binary(PersonId)}]}),
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
            Success = ghostlight_db:update_person(PersonRecord),
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

