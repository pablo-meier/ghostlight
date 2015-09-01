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
         json_to_record/1,

         validate_show/1
        ]).

-export([record_to_json/1,
         record_to_proplist/1]).

-include("apps/ghostlight/include/ghostlight_data.hrl").

%%% Callback API

get_html(ShowId) ->
    ShowRecord = ghostlight_db:get_show(ShowId),
    Proplist = record_to_proplist(ShowRecord),
    make_detail_proplist(Proplist).

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


%%% Data conversion

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

    ghostlight_utils:proplist_with_valid_values(
      [{id, ShowId},
       {title, Title},
       {producers, [ ghostlight_utils:person_or_org_record_to_proplist(Producer) || Producer <- Producers ]},
       {special_thanks, SpecialThanks},
       {dates, Dates},
       {opening, FirstDate},
       {closing, LastDate},
       {hosts, [ ghostlight_people:record_to_proplist(Host) || Host <- Hosts]},
       {press, [ [{link, Url}, {label, Label}] || #press_link{link=Url, label=Label} <- PressLinks]},
       {description, Description},
       {links, ghostlight_utils:external_links_record_to_proplist(ExternalLinks)},
       {performances, [ performance_to_proplists(Performance) || Performance <- Performances ] }
      ]).


performance_to_proplists(#performance{ 
                             work=Work,
                             onstage=Onstage,
                             offstage=Offstage,
                             directors=Directors,
                             directors_note=DirectorsNote,
                             description=Description}) ->
    ghostlight_utils:proplist_with_valid_values(
      [{work, ghostlight_work:record_to_proplist(Work)},
       {directors_note, DirectorsNote},
       {description, Description},
       {directors, [ ghostlight_people:record_to_proplist(Director) || Director <- Directors ]},
       {onstage, [ onstage_as_proplist(Performer) || Performer <- Onstage ]},
       {offstage, [ offstage_as_proplist(Person) || Person <- Offstage ]}]).


onstage_as_proplist(#onstage{ person=Person, role=Role}) ->
    [{role, Role}] ++ ghostlight_people:record_to_proplist(Person).
offstage_as_proplist(#offstage{ contributor=Person, jobs=Jobs}) ->
    [{jobs, Jobs}] ++ ghostlight_people:record_to_proplist(Person).


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
                   jobs=Jobs,
                   contributor=Person
                }) ->
    ghostlight_utils:json_with_valid_values([
        {<<"job">>, Jobs},
        {<<"contributor">>, ghostlight_people:record_to_json(Person)}
    ]).

json_to_record(Decoded) ->
    ShowId = proplists:get_value(<<"id">>, Decoded, null),
    Title = proplists:get_value(<<"title">>, Decoded),
    SpecialThanks = proplists:get_value(<<"special_thanks">>, Decoded),
    ShowDescription = proplists:get_value(<<"description">>, Decoded),
    Dates = lists:map(fun iso8601:parse/1, proplists:get_value(<<"dates">>, Decoded, [])),
    Producers = [ ghostlight_utils:person_or_org_json_to_record(Producer)
                  || Producer <- proplists:get_value(<<"producers">>, Decoded, [])],
    Performances = lists:map(fun performance_json_to_record/1, proplists:get_value(<<"performances">>, Decoded, [])),

    ExternalLinks = ghostlight_utils:external_links_json_to_record(Decoded),
    Hosts = [ ghostlight_people:json_to_record(Host) || Host <- proplists:get_value(<<"hosts">>, Decoded, []) ],

    Press = proplists:get_value(<<"press">>, Decoded, []),
    PressLinks = [ #press_link{link=proplists:get_value(<<"link">>, Link, null),
                               label=proplists:get_value(<<"label">>, Link, null)} || Link <- Press],

    Vanity = ghostlight_utils:vanity_name_json_to_binary(Decoded),

    validate_show(#show{
        id=ShowId,
        title=Title,
        vanity_name=Vanity,
        special_thanks=SpecialThanks,
        description=ShowDescription,
        dates=Dates,
        producers=Producers,
        hosts=Hosts,
        performances=Performances,
        external_links=ExternalLinks,
        press_links=PressLinks
    }).

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
    Role = proplists:get_value(<<"role">>, Onstage, null),
    #onstage{
      person = Performer,
      role = Role
    }.

offstage_json_to_record(Offstage) ->
    Contributor = ghostlight_utils:person_or_org_json_to_record(proplists:get_value(<<"contributor">>, Offstage)),
    Jobs = case proplists:get_value(<<"jobs">>, Offstage) of
               X when is_list(X) -> X;
               X when is_binary(X) -> [X]
           end,
    #offstage{
      contributor = Contributor,
      jobs = Jobs
    }.

%%% Helpers


%%% @doc 
%%% The templates need to have a bit more data when deciding how to present information,
%%% this function adds properties to the proplist that are specific to the detail HTML template
%%% and won't be needed if others call `record_to_proplist` on the data type.
%%% @end
make_detail_proplist(Proplist) ->
    Performances = proplists:get_value(performances, Proplist),
    TaggedForRoles = [ tag_roles_in_cast(Performance) || Performance <- Performances ],
    substitute_property(performances, Proplist, TaggedForRoles).


tag_roles_in_cast(Performance) ->
    Onstage = proplists:get_value(onstage, Performance, []),
    NoRoles = lists:all(fun (X) -> proplists:get_value(role, X) =:= null end, Onstage),
    case NoRoles of
        true -> Performance ++ [{no_roles, true}];
        false -> Performance
    end.


substitute_property(Key, Lst, Replacement) ->
    proplists:delete(Key, Lst) ++ [{Key, Replacement}].


%% Shows have the following restrictions:
%% * Must have either an ID (existing) or a name (new show).
%% * Must have at least one producer.
%% * Must have at least one showdate.
%% * Must have at least one performance.
%% * Each Producer must be a valid Person or Org.
%% * Each Performance must contain a valid work.
%% * Every offstage contributor must have at least one job.
%% * Must have a valid External Links.
validate_show(#show{ id = null, title = null }) ->
    throw(show_missing_identifying_information);
validate_show(S=#show{ id = null, title = _ }) ->
    validate_body(S);
validate_show(S=#show{ id = Id }) ->
    case ghostlight_db_utils:is_valid_uuid(Id) of
        true -> validate_body(S);
        false -> throw(not_valid_uuid)
    end.

validate_body(S=#show{
    vanity_name = Vanity,
    producers = Producers,
    performances = Performances,   % length > 1
    external_links = EL,
    dates = Dates
}) ->
    ghostlight_utils:ensure_minimum_length(Producers, 1, <<"Must have at least one producer">>),
    ghostlight_utils:ensure_minimum_length(Performances, 1, <<"Must have at least one performance">>),
    ghostlight_utils:ensure_minimum_length(Dates, 1, <<"Must have at least one date">>),

    [ ghostlight_utils:validate_person_or_org(Producer) || Producer <- Producers ],
    [ validate_performance(Performance) || Performance <- Performances ],

    ghostlight_utils:validate_external_links(EL),
    ghostlight_utils:validate_vanity_name(Vanity),
    S.

validate_performance(#performance{
    id = Id,
    work = Work,
    onstage = Onstage,
    offstage = Offstage,
    directors = Directors
}) ->
    ghostlight_work:validate_work(Work),
    [ ghostlight_people:validate_person(Director) || Director <- Directors ],
    [ ghostlight_people:validate_person(Person) || #onstage{person=Person} <- Onstage],

    [ ghostlight_utils:validate_person_or_org(Creator) || #offstage{contributor=Creator} <- Offstage],
    [ ghostlight_utils:ensure_minimum_length(Jobs, 1, <<"Offstage contributors must have at least one job.">>)
        || #offstage{ jobs = Jobs } <- Offstage],

    case {Id, ghostlight_db_utils:is_valid_uuid(Id)} of
        {null, _} -> ok;
        {_, true} -> ok;
        {_, false} -> throw(not_valid_uuid)
    end.
