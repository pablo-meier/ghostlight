-module(ghostlight_people).

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


get_html(PersonId) ->
    PersonRecord = ghostlight_db:get_person(PersonId),
    record_to_proplist(PersonRecord).

get_listings_html() ->
    PersonList = ghostlight_db:get_person_listings(),
    [{people, [ record_to_proplist(Person) || Person <- PersonList ]}].

edit_html(PersonId) ->
    PersonRecord = ghostlight_db:get_person(PersonId, markdown),
    AsJson = jsx:encode(record_to_json(PersonRecord)),
    [{name, PersonRecord#person_return.person#person.name},
     {editmode, AsJson}].


get_listings_json() ->
    PersonList = ghostlight_db:get_person_listings(),
    [{<<"people">>, [ record_to_json(Person) || Person <- PersonList ]}].

get_prefetch() ->
    PersonList = ghostlight_db:get_person_listings(),
    [ [{<<"id">>, Id},{<<"name">>, Name}] || #person{id=Id, name=Name} <- PersonList ].

get_json(PersonId) ->
    PersonRecord = ghostlight_db:get_person(PersonId),
    record_to_json(PersonRecord).

post_json(PersonRecord) ->
    ghostlight_db:insert_person(PersonRecord).

edit_json(PersonRecord) ->
    ghostlight_db:update_person(PersonRecord).

get_id(#person{id=Id}) -> Id;
get_id(#person_return{person=#person{id=Id}}) -> Id.


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
                     orgs_employee=OrgsEmp,
                     orgs_member=OrgsMem}) ->

    OnstageProplist = [ make_onstage_proplist(OnstageShow) || OnstageShow <- Onstage ],
    OffstageProplist = [ make_offstage_proplist(OffstageShow) || OffstageShow <- Offstage ],
    ProducedProplist = [ ghostlight_show:record_to_proplist(ProducedShow) || ProducedShow <- Produced ],
    DirectorProplist = [ ghostlight_show:record_to_proplist(DirectedShow) || DirectedShow <- Directed ],
    AuthorshipProplist = [ ghostlight_work:record_to_proplist(Work) || Work <- Authored],

    OrgProplist = [ [{id, OrgId},
                     {name, OrgName},
                     {position, Position}] || #org_work{org_id=OrgId,
                                                        org_name=OrgName,
                                                        title=Position} <- OrgsEmp ],

    MemProplist = [ ghostlight_org:record_to_proplist(Org) || Org <- OrgsMem ],

    [{id, PersonId},
     {name, Name},
     {description, Description},
     {onstage_list, OnstageProplist},
     {offstage_list, OffstageProplist},
     {authorship, AuthorshipProplist},
     {director, DirectorProplist},
     {producer, ProducedProplist},
     {org_employee, OrgProplist},
     {org_member, MemProplist},
     {links, ghostlight_utils:external_links_record_to_proplist(ExternalLinks)}];

record_to_proplist(#person{
                      id=PersonId,
                      name=PersonName
                   }) ->
    [{person_name, PersonName},
     {person_id, PersonId}].


make_onstage_proplist(Show = #show{ performances=[#performance{
                                 onstage=[#onstage{ role=Role }]
                             }]}) ->
    Toplevel = ghostlight_show:record_to_proplist(Show),
    case Role of
        null -> Toplevel;
        _ -> [{role, Role}|Toplevel]
    end.


make_offstage_proplist(Show= #show{ performances=[#performance{
                                      offstage=[#offstage{ job=Job }]
                                  }]}) ->
    Toplevel = ghostlight_show:record_to_proplist(Show),
    case Job of
        null -> Toplevel;
        _ -> [{job, Job}|Toplevel]
    end.


json_to_record(Person) ->
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
         {<<"directed">>, [ ghostlight_show:record_to_json(Show) || Show <- Directed ]},
         {<<"onstage">>, [ ghostlight_show:record_to_json(Show) || Show <- Onstage ]},
         {<<"offstage">>, [ ghostlight_show:record_to_json(Show) || Show <- Offstage ]},
         {<<"organizations">>, [ org_work_to_json(Org) || Org <- Orgs ]}]
    ])).


org_work_to_json(#org_work{org_id=OrgId, org_name=Name, title=Title}) ->
    ghostlight_utils:json_with_valid_values(
      [{<<"id">>, OrgId},
       {<<"name">>, Name},
       {<<"title">>, Title}]).


record_to_json_shared(#person{
                          id=PersonId,
                          name=PersonName,
                          description=Description,
                          external_links=Links}) ->
    [
     {<<"id">>, PersonId},
     {<<"name">>, PersonName},
     {<<"description">>, Description},
     {<<"social">>, ghostlight_utils:external_links_record_to_json(Links)}
    ].
