-module(ghostlight_work).

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

         validate_work/1
        ]).

-export([record_to_json/1,
         record_to_proplist/1]).

-include("apps/ghostlight/include/ghostlight_data.hrl").


get_html(WorkId) ->
    WorkRecord = ghostlight_db:get_work(WorkId, html),
    record_to_proplist(WorkRecord).

get_listings_html() ->
    Listings = ghostlight_db:get_work_listings(),
    [{<<"works">>, [ record_to_proplist(Work) || Work <- Listings ]}].

edit_html(WorkId) ->
    WorkRecord = ghostlight_db:get_work(WorkId, markdown),
    AsJsonProplist = record_to_json(WorkRecord),
    AsJson = jsx:encode(proplists:get_value(<<"work">>, AsJsonProplist)),
    [{title, WorkRecord#work_return.work#work.title},
     {editmode, AsJson}].

get_listings_json() ->
    Listings = ghostlight_db:get_work_listings(),
    [{<<"shows">>, [ record_to_json(Work) || Work <- Listings ]}].

get_prefetch() ->
    WorkList = ghostlight_db:get_work_listings(),
    [ [{<<"id">>, Id},{<<"title">>, Title}] || #work{id=Id, title=Title} <- WorkList ].

get_json(WorkId) ->
    WorkRecord = ghostlight_db:get_work(WorkId, markdown),
    record_to_json(WorkRecord).

post_json(WorkRecord) ->
    ghostlight_db:insert_work(WorkRecord).

edit_json(WorkRecord) ->
    ghostlight_db:update_work(WorkRecord).

get_id(#work{id=Id}) -> Id.


record_to_proplist(#work_return{
                       work=Work,
                       shows=Shows}) ->

  ShowsProplist = [ [{show_id, ShowId},
                     {show_title, ShowTitle},
                     {producers, [ ghostlight_utils:person_or_org_record_to_proplist(Producer) ||
                                   Producer <- Producers ]}
                    ] || #show{
                             id=ShowId,
                             title=ShowTitle,
                             producers=Producers
                         } <- Shows ],

  record_to_proplist(Work) ++ [{shows, ShowsProplist}];

record_to_proplist(#work{
                      id = WorkId,
                      title=WorkTitle,
                      authors=Authors,
                      description = Description,
                      minutes_long = MinutesLong
                   }) ->
    [{id, WorkId},
     {title, WorkTitle},
     {authors, [ ghostlight_utils:person_or_org_record_to_proplist(Author) || Author <- Authors ]},
     {description, Description},
     {minutes_long, MinutesLong}].


record_to_json(#work{
                  id=WorkId,
                  title=WorkTitle,
                  authors=WorkAuthors,
                  description=Description,
                  minutes_long=MinutesLong
               }) ->
    ghostlight_utils:json_with_valid_values([
        {<<"id">>, WorkId},
        {<<"title">>, WorkTitle},
        {<<"description">>, Description},
        {<<"minutes_long">>, MinutesLong},
        {<<"authors">>, [ ghostlight_utils:person_or_org_record_to_json(Author) || Author <- WorkAuthors ]}
    ]);


record_to_json(#work_return{
                  work=Work,
                  shows=Shows
               }) ->
    ghostlight_utils:json_with_valid_values([
        {<<"work">>, record_to_json(Work)},
        {<<"shows">>, [ ghostlight_show:record_to_json(Show) || Show <- Shows ]}
    ]).


json_to_record(Proplist) ->
    CollabOrg = case proplists:get_value(<<"collaborating_org">>, Proplist, null) of
                    null -> [];
                    Org -> [ghostlight_org:json_to_record(Org)]
                end,
    #work {
       id = proplists:get_value(<<"id">>, Proplist, null),
       title = proplists:get_value(<<"title">>, Proplist),
       vanity_name = ghostlight_utils:vanity_name_json_to_binary(Proplist),
       authors = [ ghostlight_utils:person_or_org_json_to_record(Author)
                   || Author <- proplists:get_value(<<"authors">>, Proplist, []) ],
       description = proplists:get_value(<<"description">>, Proplist, null),
       minutes_long = proplists:get_value(<<"minutes_long">>, Proplist, null),
       collaborating_orgs = CollabOrg
    }.


validate_work(#work{ id = null, title = null }) ->
    throw(work_missing_identifying_information);
validate_work(W=#work{ id = null, title = _ }) ->
    validate_work_body(W);
validate_work(W=#work{ id = Id, title = _ }) ->
    case ghostlight_db_utils:is_valid_uuid(Id) of
        true -> validate_work_body(W);
        false -> throw(not_valid_uuid)
    end.

validate_work_body(W=#work {
                        authors = Authors,
                        vanity_name = Vanity
                     }) ->
    ghostlight_utils:validate_vanity_name(Vanity),
    [ validate_author(Author) || Author <- Authors ],
    W.

validate_author(P=#person{}) -> ghostlight_people:validate_person(P);
validate_author(O=#organization{}) -> ghostlight_org:validate_org(O).
