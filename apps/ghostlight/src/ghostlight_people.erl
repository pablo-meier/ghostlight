-module(ghostlight_people).
-export([init/2,
         content_types_provided/2,
         content_types_accepted/2,
         allowed_methods/2]).

-export([person_to_html/2,
         person_to_json/2]).

-export([json_to_record/1,
         record_to_json/1,
         record_to_proplist/1,
        
         post_json/2]).

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
        undefined ->
            PersonList = ghostlight_db:get_person_listings(),
            ForTemplate = [{people, [ record_to_proplist(Person) || Person <- PersonList ]}],
            {ok, Body} = person_listing_template:render(ForTemplate),
            {Body, Req, State};

        <<"new">> ->
            {ok, Body} = insert_person_template:render([]),
            {Body, Req, State};
        _ ->
            PersonRecord = ghostlight_db:get_person(PersonId),
            ForTemplate = record_to_proplist(PersonRecord),
            {ok, Body} = person_template:render(ForTemplate),
            {Body, Req, State}
    end.

%% Makes the Record returned from the DB into a proplist we can feed the template.
%% Aw hell yeah Pattern Matching.
record_to_proplist(#person_return{
                     name=Name,
                     authored=Authored,
                     directed=Directed,
                     onstage=Onstage,
                     offstage=Offstage,
                     orgs=Orgs}) ->

    OnstageProplist = [ [{show_id, ShowId},
                         {show_title, ShowTitle},
                         {org_id, OrgId},
                         {org_name, OrgName},
                         {work_id, WorkId},
                         {work_title, WorkTitle},
                         {role, ghostlight_utils:remove_null(Role)}]
                            || #show{ title=ShowTitle,
                                      id=ShowId,
                                      org=#organization{id=OrgId, name=OrgName},
                                      performances=[#performance{
                                                      work=#work{ id=WorkId, title=WorkTitle },
                                                      onstage=#onstage{ role=Role }}]
                                    } <- Onstage],
    OffstageProplist = [ [{show_id, ShowId}, 
                          {show_title, ShowTitle},
                          {org_id, OrgId},
                          {org_name, OrgName},
                          {work_id, WorkId},
                          {work_title, WorkTitle},
                          {job, Job}] || #show{ title=ShowTitle,
                                                id=ShowId,
                                                org=#organization{id=OrgId, name=OrgName},
                                                performances=[#performance{
                                                                work=#work{ id=WorkId, title=WorkTitle },
                                                                offstage=#offstage{ job=Job }
                                                             }]
                                              } <- Offstage],
    AuthorshipProplist = [ [{work_id, WorkId},
                            {title, Title}] || #work{id=WorkId, title=Title} <- Authored],

    DirectorProplist = [[{show_id, ShowId},
                         {show_title, ShowTitle},
                         {org_id, OrgId},
                         {org_name, OrgName},
                         {work_id, WorkId},
                         {work_title, WorkTitle}] || #show{ title=ShowTitle,
                                                            id=ShowId,
                                                            org=#organization{id=OrgId, name=OrgName},
                                                            performances=[#performance{
                                                                            work=#work{ id=WorkId, title=WorkTitle }
                                                                         }]} <- Directed ],

    OrgProplist = [ [{org_id, OrgId},
                     {org_name, OrgName},
                     {position, Position}] || #org_work{org_id=OrgId, org_name=OrgName, title=Position} <- Orgs ],

      
    [{name, Name},
     {onstage_list, OnstageProplist},
     {offstage_list, OffstageProplist},
     {authorship, AuthorshipProplist},
     {director, DirectorProplist},
     {organizations, OrgProplist}];

record_to_proplist(#person{
                      id=PersonId,
                      name=PersonName
                   }) ->
    [{person_name, PersonName},
     {person_id, PersonId}].


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


record_to_json(#person{
                  id=PersonId,
                  name=PersonName
               }) ->
    ghostlight_utils:json_with_valid_values([
        {<<"person_id">>, PersonId},
        {<<"name">>, PersonName}
    ]);
record_to_json(#person_return{
                     id=PersonId,
                     name=Name,
                     authored=Authored,
                     directed=Directed,
                     onstage=Onstage,
                     offstage=Offstage,
                     orgs=Orgs
               }) ->
    ghostlight_utils:json_with_valid_values([
        {<<"id">>, PersonId},
        {<<"name">>, Name},
        {<<"authored">>, [ ghostlight_work:record_to_json(Work) || Work <- Authored ]},
        {<<"directed">>, [ ghostlight_work:record_to_json(Work) || Work <- Directed ]},
        {<<"onstage">>, [ ghostlight_show:record_to_json(Show) || Show <- Onstage ]},
        {<<"offstage">>, [ ghostlight_show:record_to_json(Show) || Show <- Offstage ]},
        {<<"organizations">>, [ ghostlight_org:record_to_json(Org) || Org <- Orgs ]}
    ]).


post_json(Req, State) ->
    {ok, RequestBody, Req2} = cowboy_req:body(Req),
    AsJson = jiffy:decode(RequestBody),
    PersonRecord = json_to_record(AsJson),
    case PersonRecord#person.id of
        null ->
            PersonId = ghostlight_db:insert_person(PersonRecord),
            Response = jiffy:encode({[{<<"status">>, <<"ok">>}, {<<"id">>, list_to_binary(PersonId)}]}),
            {true, cowboy_req:set_resp_body(Response, Req2), State};
        _Else ->
            Body = jiffy:encode({[{<<"error">>, <<"You may not insert a person with the field 'id'.">>}]}),
            Req3 = cowboy_req:set_resp_body(Body, Req2),
            {false, Req3, State}
    end.


