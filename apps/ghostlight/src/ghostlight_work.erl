-module(ghostlight_work).

-export([author_type_to_binary/1,
         author_type_to_atom/1]).

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
     {authors, [ [{author, ghostlight_utils:person_or_org_record_to_proplist(Author)},
                  {byline, write_byline(Types)}] || #authorship { author = Author, types = Types}  <- Authors ]},
     {description, Description},
     {minutes_long, MinutesLong}].


-spec write_byline(list(#authorship{})) -> binary().
write_byline(Types) ->
    ExcludingWritten = lists:filter(fun (X) -> X =/= written end, Types),
    sophisticated_byline(ExcludingWritten).


sophisticated_byline([]) -> <<"">>;
sophisticated_byline(Types) ->
    Mapped = [ binary_to_list(author_type_to_binary(T)) || T <- Types ],
    case length(Mapped) of
        1 -> list_to_binary(hd(Mapped));
        2 -> list_to_binary(hd(Mapped) ++ " & " ++ hd(tl(Mapped)));
        X -> 
            Last = lists:last(Mapped),
            AllButLast = lists:sublist(Mapped, X - 1),
            list_to_binary(string:join(AllButLast, ", ") ++ ", and " ++ Last)
    end.


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
        {<<"authors">>, [ authorship_to_json(Author) || Author <- WorkAuthors ]}
    ]);


record_to_json(#work_return{
                  work=Work,
                  shows=Shows
               }) ->
    ghostlight_utils:json_with_valid_values([
        {<<"work">>, record_to_json(Work)},
        {<<"shows">>, [ ghostlight_show:record_to_json(Show) || Show <- Shows ]}
    ]).


authorship_to_json(#authorship {
                      author = Author,
                      types = Types
                   }) ->
    Base = ghostlight_utils:person_or_org_record_to_json(Author),
    Base ++ [{<<"types">>, [ author_type_to_binary(T) || T <- Types ]}].



json_to_record(Proplist) ->
    validate_work(#work {
       id = proplists:get_value(<<"id">>, Proplist, null),
       title = proplists:get_value(<<"title">>, Proplist),
       vanity_name = ghostlight_utils:vanity_name_json_to_binary(Proplist),
       authors = [ json_authors_to_record(Author)
                   || Author <- proplists:get_value(<<"authors">>, Proplist, []) ],
       description = proplists:get_value(<<"description">>, Proplist, null),
       minutes_long = proplists:get_value(<<"minutes_long">>, Proplist, null)
    }).

json_authors_to_record(Proplist) ->
    Author = ghostlight_utils:person_or_org_json_to_record(Proplist),
    Types = case proplists:get_value(<<"types">>, Proplist, null) of
                null -> [written];
                Else -> [ author_type_to_atom(X) || X <- Else ]
            end,
    #authorship {
       author = Author,
       types = Types
    }.


-spec author_type_to_atom(binary()) -> authorship_type().
author_type_to_atom(<<"written">>) -> written;
author_type_to_atom(<<"Written">>) -> written;
author_type_to_atom(<<"book">>) -> book;
author_type_to_atom(<<"Book">>) -> book;
author_type_to_atom(<<"music">>) -> music;
author_type_to_atom(<<"Music">>) -> music;
author_type_to_atom(<<"Lyrics">>) -> lyrics;
author_type_to_atom(<<"lyrics">>) -> lyrics;
author_type_to_atom(<<"Choreography">>) -> choreography;
author_type_to_atom(<<"choreography">>) -> choreography;
author_type_to_atom(Else) when is_list(Else) -> {other, list_to_binary(Else)};
author_type_to_atom(Else) when is_binary(Else) -> {other, Else};
author_type_to_atom(_) ->
    throw(bad_author_type).


-spec author_type_to_binary(authorship_type()) -> binary().
author_type_to_binary(written) -> <<"Written">>;
author_type_to_binary(book) -> <<"Book">>;
author_type_to_binary(music) -> <<"Music">>;
author_type_to_binary(lyrics) -> <<"Lyrics">>;
author_type_to_binary(choreography) -> <<"Choreography">>;
author_type_to_binary({other, Else}) -> Else;
author_type_to_binary(_) -> throw(unknown_author_type).


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
    ghostlight_utils:ensure_minimum_length(Authors, 1, <<"Must have at least one author.">>),
    [ ghostlight_utils:validate_person_or_org(Author) || #authorship{ author=Author } <- Authors ],
    W.
