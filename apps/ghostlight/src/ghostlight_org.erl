-module(ghostlight_org).
-export([init/2,
         content_types_provided/2,
         content_types_accepted/2,
         allowed_methods/2]).
-export([org_to_html/2,
         org_to_json/2]).

-export([json_to_record/1]).

-include("apps/ghostlight/include/ghostlight_data.hrl").

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"DELETE">>],
     Req, State}.
content_types_provided(Req, State) ->
    {[
      {<<"text/html">>, org_to_html},
      {<<"application/json">>, org_to_json}
     ], Req, State}.
content_types_accepted(Req, State) ->
    {[
      {<<"application/json">>, post_json}
     ], Req, State}.

org_to_html(Req, State) ->
    OrgId = cowboy_req:binding(org_ig, Req),
    case OrgId of
        <<"new">> ->
            {ok, Body} = insert_org_template:render([]),
            {Body, Req, State};
        _ ->
            OrgRecord = ghostlight_db:get_org(OrgId),
            lager:info("OrgRecord returned from DB is ~p~n", [OrgRecord]),
            ForTemplate = record_to_proplist(OrgRecord),
            {ok, Body} = person_template:render(ForTemplate),
            {Body, Req, State}
    end.

%% Makes the Record returned from the DB into a proplist we can feed the template.
%% Aw hell yeah Pattern Matching.
record_to_proplist(#person_return{
                     name=Name,
                     authored=Authored,
                     directed=_Directed,
                     onstage=Onstage,
                     offstage=Offstage,
                     orgs=Orgs}) ->

    OnstageProplist = [ [{show_id, ShowId},
                         {show_title, ShowTitle},
                         {org_id, OrgId},
                         {org_name, OrgName},
                         {work_id, WorkId},
                         {work_title, WorkTitle},
                         {role, Role}] || #show{ title=ShowTitle,
                                                 id=ShowId,
                                                 org=#organization{id=OrgId, name=OrgName},
                                                 performances=[#performance{
                                                                 work=#work{ id=WorkId, title=WorkTitle },
                                                                 onstage=#onstage{ role=Role }
                                                              }]} <- Onstage],
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

    OrgProplist = [ [{org_id, OrgId},
                     {org_name, OrgName},
                     {position, Position}] || #org_work{org_id=OrgId, org_name=OrgName, title=Position} <- Orgs ],
      
    [{name, Name},
     {onstage_list, OnstageProplist},
     {offstage_list, OffstageProplist},
     {authorship, AuthorshipProplist},
     {organizations, OrgProplist}].


json_to_record({Person}) ->
    PersonId = proplists:get_value(<<"id">>, Person, null),
    PersonName = proplists:get_value(<<"name">>, Person, null),
    #person{
       id = PersonId,
       name = PersonName
    }.

org_to_json(Req, State) ->
    {<<"{ \"status\": \"ok\" }">>, Req, State}.

