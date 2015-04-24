-module(ghostlight_org).
-export([init/2,
         content_types_provided/2,
         content_types_accepted/2,
         charsets_provided/2,
         allowed_methods/2]).

-export([org_to_html/2,
         org_to_json/2]).

-export([json_to_record/1,
         record_to_json/1,

         to_json/2]).

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
charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.
content_types_provided(Req, State) ->
    {[
      {<<"text/html">>, org_to_html},
      {<<"application/json">>, org_to_json}
     ], Req, State}.
content_types_accepted(Req, State) ->
    {[
      {<<"application/json; charset=utf-8">>, to_json},
      {<<"application/json">>, to_json}
     ], Req, State}.

org_to_html(Req, State) ->
    OrgId = cowboy_req:binding(org_id, Req),
    Command = cowboy_req:binding(command, Req),
    case {OrgId, Command} of
        {undefined, undefined} ->
            OrgList = ghostlight_db:get_org_listings(),
            ForTemplate = [{orgs, [ record_to_proplist(Org) || Org <- OrgList ]}],
            {ok, Body} = org_listing_template:render(ForTemplate),
            {Body, Req, State};
        {<<"new">>, _} ->
            {ok, Body} = insert_org_template:render([]),
            {Body, Req, State};
        {_, undefined} ->
            OrgRecord = ghostlight_db:get_org(OrgId),
            ForTemplate = record_to_proplist(OrgRecord),
            {ok, Body} = org_template:render(ForTemplate),
            {Body, Req, State};
        {_, <<"edit">>} ->
            OrgRecord = ghostlight_db:get_org(OrgId, markdown),
            {AsOuterBodyJson} = record_to_json(OrgRecord),
            AsJson = jiffy:encode(proplists:get_value(<<"org">>, AsOuterBodyJson)),
            {ok, Body} = insert_org_template:render([{name, OrgRecord#org_return.org#organization.name},
                                                     {editmode, AsJson}]),
            {Body, Req, State}
    end.


%% Makes the Record returned from the DB into a proplist we can feed the template.
%% Aw hell yeah Pattern Matching.
record_to_proplist(#org_return{
                     org=#organization{
                         id=OrgId,
                         name=Name,
                         tagline=Tagline,
                         description=Description,
                         employees=Employees,
                         members=Members,
                         external_links=ExternalLinks
                     },
                     shows_produced=Shows}) ->
  ShowProplist = [ [{show_id, ShowId},
                    {show_title, ShowTitle},
                    {performances, [ [{work_id, WorkId}, {work_title, WorkTitle}] 
                                       || #performance{work=#work{id=WorkId, title=WorkTitle}} <- Performances ]},
                    {first_show, lists:last(Dates)},
                    {last_show, lists:nth(1, Dates)}
                   ] || #show{ id=ShowId,
                               title=ShowTitle,
                               performances=Performances,
                               dates=Dates
                             } <- Shows ],
  EmployeesProplist = [ [{person_id, PersonId},
                         {person_name, PersonName},
                         {person_description, ghostlight_utils:remove_null(EmpDescription)},
                         {title, Title}] || #org_employee{ title=Title,
                                                           description=EmpDescription,
                                                           person=#person{
                                                                     id=PersonId,
                                                                     name=PersonName
                                                                    }
                                                         } <- Employees ],
  MemberProplist = [ [{person_id, PersonId},
                      {person_name, PersonName},
                      {person_description, ghostlight_utils:remove_null(MemDescription)}] 
                        || #org_member { description=MemDescription,
                                         member=#person{
                                             id=PersonId,
                                             name=PersonName
                                         }
                                        } <- Members],
    [{id, OrgId},
     {name, Name},
     {tagline, Tagline},
     {description, Description},
     {shows, ShowProplist},
     {employees, EmployeesProplist},
     {members, MemberProplist},
     {links, ghostlight_utils:external_links_record_to_proplist(ExternalLinks)}];

record_to_proplist(#organization{
                       id=Id,
                       name=Name,
                       tagline=Tagline,
                       description=Description
                   }) ->
    [{org_id, Id},
     {name, Name},
     {tagline, ghostlight_utils:remove_null(Tagline)},
     {description, ghostlight_utils:remove_null(Description)}].


record_to_json(#organization{
                  id=OrgId,
                  name=OrgName,
                  tagline=OrgTagline,
                  description=Description,
                  date_founded=DateFounded,
                  members=Members,
                  employees=Employees,
                  external_links=ExternalLinks
}) ->
    ghostlight_utils:json_with_valid_values([
        {<<"id">>, OrgId},
        {<<"name">>, OrgName},
        {<<"tagline">>, OrgTagline},
        {<<"description">>, Description},
        {<<"date_founded">>, ghostlight_utils:erl_date_to_iso8601(DateFounded)},
        {<<"members">>, [ member_to_json(Member) || Member <- Members]},
        {<<"employees">>, [ employee_to_json(Employee) || Employee <- Employees ]},
        {<<"social">>, ghostlight_utils:external_links_record_to_json(ExternalLinks)}
    ]);

record_to_json(#org_return{
                  org=Org,
                  shows_produced=Shows
}) ->
    ghostlight_utils:json_with_valid_values([
        {<<"org">>, record_to_json(Org)},
        {<<"shows_produced">>, [ ghostlight_show:record_to_json(Show) || Show <- Shows ]} 
    ]).


json_to_record({Organization}) ->
    OrgId = proplists:get_value(<<"id">>, Organization, null),
    OrgName = proplists:get_value(<<"name">>, Organization, null),
    OrgDescription = proplists:get_value(<<"description">>, Organization, null),
    OrgTagline = proplists:get_value(<<"tagline">>, Organization, null),

    Members = proplists:get_value(<<"members">>, Organization, []),
    DecodedMembers = [ decode_member(Member) || Member <- Members ],

    Employees = proplists:get_value(<<"employees">>, Organization, []),
    DecodedEmployees = [ decode_employee(Emp) || Emp <- Employees ],

    ExternalLinks = ghostlight_utils:external_links_json_to_record(Organization),

    #organization{
       id=OrgId,
       name=OrgName,
       tagline=OrgTagline,
       description=OrgDescription,
       members=DecodedMembers,
       employees=DecodedEmployees,
       external_links=ExternalLinks
    }.

decode_member({Member}) ->
    Description = proplists:get_value(<<"description">>, Member, null),
    Person = ghostlight_people:json_to_record(proplists:get_value(<<"person">>, Member)),
    #org_member{
       member=Person,
       description=Description
    }.
decode_employee({Emp}) ->
    Description = proplists:get_value(<<"description">>, Emp, null),
    Title = proplists:get_value(<<"title">>, Emp, null),
    Person = ghostlight_people:json_to_record(proplists:get_value(<<"person">>, Emp)),
    #org_employee{
       person=Person,
       title=Title,
       description=Description
    }.

member_to_json(#org_member{member=Person, description=Desc}) ->
    ghostlight_utils:json_with_valid_values([
        {<<"person">>, ghostlight_people:record_to_json(Person)},
        {<<"description">>, Desc}
    ]).
employee_to_json(#org_employee{person=Person, title=Title, description=Desc}) ->
    ghostlight_utils:json_with_valid_values([
        {<<"person">>, ghostlight_people:record_to_json(Person)},
        {<<"title">>, Title},
        {<<"description">>, Desc}
    ]).

org_to_json(Req, State) ->
    OrgId = cowboy_req:binding(org_id, Req),
    case OrgId of
        undefined ->
            OrgList = ghostlight_db:get_org_listings(),
            ToEncode = {[{<<"organizations">>, [ record_to_json(Org) || Org <- OrgList ]}]},
            Body = jiffy:encode(ToEncode),
            {Body, Req, State};
        _ ->
            OrgRecord = ghostlight_db:get_org(OrgId, markdown),
            AsJson = jiffy:encode(record_to_json(OrgRecord)),
            {AsJson, Req, State}
    end.


to_json(Req, State) ->
    {ok, RequestBody, Req2} = cowboy_req:body(Req),
    AsJson = jiffy:decode(RequestBody),
    OrgRecord = json_to_record(AsJson),
    {Method, Req3} = cowboy_req:method(Req2),
    case {OrgRecord#organization.id, Method} of
        {null, <<"POST">>} ->
            OrgId = ghostlight_db:insert_org(OrgRecord),
            Response = jiffy:encode({[{<<"status">>, ok}, {<<"id">>, list_to_binary(OrgId)}]}),
            {true, cowboy_req:set_resp_body(Response, Req3), State};
        {_Else, <<"POST">>} ->
            Body = jiffy:encode({[{<<"error">>, <<"You may not insert an organization with the field 'id'.">>}]}),
            Req4 = cowboy_req:set_resp_body(Body, Req3),
            {false, Req4, State};
        {null, <<"PUT">>} ->
            Body = jiffy:encode({[{<<"error">>, <<"You must PUT on an existing resource.">>}]}),
            Req4 = cowboy_req:set_resp_body(Body, Req3),
            {false, Req4, State};
        {_OrgId, <<"PUT">>} ->
            Success = ghostlight_db:update_org(OrgRecord),
            case Success of
                true ->
                    Response = jiffy:encode({[{<<"status">>, ok}]}),
                    {true, cowboy_req:set_resp_body(Response, Req3), State};
                false ->
                    Body = jiffy:encode({[{<<"error">>, <<"An error occurred.">>}]}),
                    Req4 = cowboy_req:set_resp_body(Body, Req3),
                    {false, Req4, State}
            end
    end.


