-module(ghostlight_show).

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

get_html(ShowId) ->
    ShowRecord = ghostlight_db:get_show(ShowId),
    record_to_proplist(ShowRecord).

get_listings_html() ->
    ShowList = ghostlight_db:get_show_listings(),
    [{shows, [ record_to_proplist(Show) || Show <- ShowList ]}].

edit_html(ShowId) ->
    ShowRecord = ghostlight_db:get_show(ShowId, markdown),
    AsJson = jsx:encode(record_to_json(ShowRecord)),
    [{name, ShowRecord#show.title},
     {editmode, AsJson}].

get_listings_json() ->
    ShowList = ghostlight_db:get_show_listings(),
    [{<<"shows">>, [ record_to_json(Show) || Show <- ShowList ]}].

get_prefetch() ->
    ShowList = ghostlight_db:get_show_listings(),
    [ [{<<"id">>, Id},{<<"title">>, Title}] || #show{id=Id, title=Title} <- ShowList ].

get_json(ShowId) ->
    ShowRecord = ghostlight_db:get_show(ShowId),
    record_to_json(ShowRecord).

post_json(ShowRecord) ->
    ghostlight_db:insert_show(ShowRecord).

edit_json(ShowRecord) ->
    ghostlight_db:update_show(ShowRecord).

get_id(#show{id=Id}) -> Id.

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

    FirstDate = lists:nth(1, Dates),
    LastDate = lists:last(Dates),

    [{id, ShowId},
     {title, Title},
     {producers, [ producer_to_proplist(Producer) || Producer <- Producers ]},
     {special_thanks, SpecialThanks},
     {dates, Dates},
     {opening, FirstDate},
     {closing, LastDate},
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
                  producers=Producers,
                  performances=Performances,
                  special_thanks=SpecialThanks,
                  dates=Dates}) ->
    ghostlight_utils:json_with_valid_values([
        {<<"id">>, ShowId},
        {<<"title">>, ShowTitle},
        {<<"producers">>, [ ghostlight_utils:person_or_org_record_to_json(Producer) || Producer <- Producers ]},
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

json_to_record(Decoded) ->
    ShowId = proplists:get_value(<<"id">>, Decoded, undefined),
    Title = proplists:get_value(<<"title">>, Decoded),
    SpecialThanks = proplists:get_value(<<"special_thanks">>, Decoded),
    Dates = lists:map(fun iso8601:parse/1, proplists:get_value(<<"dates">>, Decoded, [])),
    Producers = [ ghostlight_utils:person_or_org_json_to_record(Producer)
                  || Producer <- proplists:get_value(<<"producers">>, Decoded, [])],
    Performances = lists:map(fun performance_json_to_record/1, proplists:get_value(<<"performances">>, Decoded, [])),

    ExternalLinks = ghostlight_utils:external_links_json_to_record(Decoded),
    Hosts = [ ghostlight_people:json_to_record(Host) || Host <- proplists:get_value(<<"hosts">>, Decoded, []) ],

    Press = proplists:get_value(<<"press">>, Decoded, []),
    PressLinks = [ #press_link{link=proplists:get_value(<<"link">>, Link, null),
                               description=proplists:get_value(<<"description">>, Link, null)} || {Link} <- Press],

    Vanity = ghostlight_utils:vanity_name_json_to_binary(Decoded),

    #show{
        id=ShowId,
        title=Title,
        vanity_name=Vanity,
        special_thanks=SpecialThanks,
        dates=Dates,
        producers=Producers,
        hosts=Hosts,
        performances=Performances,
        external_links=ExternalLinks,
        press_links=PressLinks
    }.

performance_json_to_record(Proplist) ->
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

onstage_json_to_record(Onstage) ->
    Performer = ghostlight_people:json_to_record(proplists:get_value(<<"performer">>, Onstage)),
    Role = proplists:get_value(<<"role">>, Onstage),
    #onstage{
      person = Performer,
      role = Role
    }.

offstage_json_to_record(Offstage) ->
    Contributor = ghostlight_utils:person_or_org_json_to_record(proplists:get_value(<<"contributor">>, Offstage)),
    Job = proplists:get_value(<<"job">>, Offstage),
    #offstage{
      contributor = Contributor,
      job = Job 
    }.
