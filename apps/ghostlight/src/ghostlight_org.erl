-module(ghostlight_org).

-export([get_html/1,
         get_listings_html/0,
         edit_html/1,

         get_listings_json/0,
         get_prefetch/0,
         get_json/1,

         get_id/1,
         post_json/1,
         edit_json/1,
         json_to_record/1
        ]).

-export([record_to_json/1,
         record_to_proplist/1]).

-include("apps/ghostlight/include/ghostlight_data.hrl").

get_html(OrgId) ->
    OrgRecord = ghostlight_db:get_org(OrgId),
    record_to_proplist(OrgRecord).

get_listings_html() ->
    OrgList = ghostlight_db:get_org_listings(),
    [{orgs, [ record_to_proplist(Org) || Org <- OrgList ]}].

edit_html(OrgId) ->
    OrgRecord = ghostlight_db:get_org(OrgId, markdown),
    {AsOuterBodyJson} = record_to_json(OrgRecord),
    AsJson = jiffy:encode(proplists:get_value(<<"org">>, AsOuterBodyJson)),
    [{name, OrgRecord#org_return.org#organization.name},
     {editmode, AsJson}].

get_listings_json() ->
    OrgList = ghostlight_db:get_org_listings(),
    [{<<"organizations">>, [ record_to_json(Org) || Org <- OrgList ]}].

get_prefetch() ->
    OrgList = ghostlight_db:get_org_listings(),
    [ {[{<<"id">>, Id},{<<"name">>, Name}]} || #organization{id=Id, name=Name} <- OrgList ].

get_json(OrgId) ->
    OrgRecord = ghostlight_db:get_org(OrgId, markdown),
    record_to_json(OrgRecord).

post_json(OrgRecord) ->
    ghostlight_db:insert_org(OrgRecord).

edit_json(OrgRecord) ->
    ghostlight_db:update_org(OrgRecord).

get_id(#organization{id=Id}) -> Id.


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

