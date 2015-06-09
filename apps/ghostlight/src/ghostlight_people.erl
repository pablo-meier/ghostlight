-module(ghostlight_people).
-behavior(ghostlight_resource_behavior).

-export([get_html/1,
         get_listings_html/0,
         edit_html/1,
        
         get_listings_json/0,
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
    AsJson = jiffy:encode(record_to_json(PersonRecord)),
    [{name, PersonRecord#person_return.person#person.name},
     {editmode, AsJson}].



get_listings_json() ->
    PersonList = ghostlight_db:get_person_listings(),
    [{<<"people">>, [ record_to_json(Person) || Person <- PersonList ]}].

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
    ProducedProplist = [ make_producer_proplist(ProducedShow) || ProducedShow <- Produced ],
    DirectorProplist = [ make_director_proplist(DirectedShow) || DirectedShow <- Directed ],
    AuthorshipProplist = [ [{work_id, WorkId},
                            {title, Title}] || #work{id=WorkId, title=Title} <- Authored],

    OrgProplist = [ [{org_id, OrgId},
                     {org_name, OrgName},
                     {position, Position}] || #org_work{org_id=OrgId,
                                                        org_name=OrgName,
                                                        title=Position} <- OrgsEmp ],
    MemProplist = [ [{org_id, OrgId},
                     {org_name, OrgName} ] || #organization{id=OrgId,
                                                            name=OrgName } <- OrgsMem ],

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


make_onstage_proplist( #show{ title=ShowTitle,
                              id=ShowId,
                              producers=Producers,
                              dates=[Opening],
                              performances=[#performance{
                                                work=#work{ id=WorkId, title=WorkTitle },
                                                onstage=[#onstage{ role=Role }]
                                            }]}) ->
    [{show_id, ShowId},
     {show_title, ShowTitle},
     {producers, [ person_or_org_to_proplist(Producer) || Producer <- Producers ]},
     {work_id, WorkId},
     {work_title, WorkTitle},
     {opening, Opening},
     {role, ghostlight_utils:remove_null(Role)}].


make_offstage_proplist( #show{ title=ShowTitle,
                               id=ShowId,
                               producers=Producers,
                               dates=[Opening],
                               performances=[#performance{
                                                 work=#work{ id=WorkId, title=WorkTitle },
                                                 offstage=[#offstage{ job=Job }]
                                             }]}) ->
    [{show_id, ShowId},
     {show_title, ShowTitle},
     {producers, [ person_or_org_to_proplist(Producer) || Producer <- Producers ]},
     {work_id, WorkId},
     {work_title, WorkTitle},
     {opening, Opening},
     {job, ghostlight_utils:remove_null(Job)}].


make_producer_proplist(#show{
                          id=ShowId,
                          title=ShowTitle,
                          dates=[Opening],
                          performances=Performances}) ->
    [{show_id, ShowId},
     {show_title, ShowTitle},
     {opening, Opening},
     {performances, [ work_proplist_from_performance(Performance) || Performance <- Performances ]} ].


make_director_proplist(#show{
                          id=ShowId,
                          title=ShowTitle,
                          producers=Producers,
                          dates=[Opening],
                          performances=[#performance{
                                            work=#work{ id=WorkId,
                                                        title=WorkTitle }
                                        }]}) ->
    [{show_id, ShowId},
     {show_title, ShowTitle},
     {work_id, WorkId},
     {work_title, WorkTitle},
     {opening, Opening},
     {producers, [ person_or_org_to_proplist(Producer) || Producer <- Producers ]} ].


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
