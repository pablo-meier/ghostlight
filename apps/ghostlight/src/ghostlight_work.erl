-module(ghostlight_work).
-export([init/2,
         content_types_provided/2,
         content_types_accepted/2,
         charsets_provided/2,
         allowed_methods/2]).
-export([work_to_html/2,
         work_to_json/2,
         
         post_json/2]).

-export([json_to_record/1,
         record_to_json/1]).

-include("apps/ghostlight/include/ghostlight_data.hrl").

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>],
     Req, State}.
charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.
content_types_provided(Req, State) ->
    {[
      {<<"text/html">>, work_to_html},
      {<<"application/json">>, work_to_json}
     ], Req, State}.
content_types_accepted(Req, State) ->
    {[
      {<<"application/json; charset=utf-8">>, post_json},
      {<<"application/json">>, post_json}
     ], Req, State}.

work_to_html(Req, State) ->
    WorkId = cowboy_req:binding(work_id, Req),
    Command = cowboy_req:binding(command, Req),
    case {WorkId, Command} of
        {undefined, undefined} ->
            Listings = ghostlight_db:get_work_listings(),
            ForTemplate = [{<<"works">>, [ record_to_proplist(Work) || Work <- Listings ]}],
            {ok, Body} = work_listing_template:render(ForTemplate),
            {Body, Req, State};
        {<<"new">>, _} ->
            {ok, Body} = insert_work_template:render([]),
            {Body, Req, State};
        {_, undefined} ->
            WorkRecord = ghostlight_db:get_work(WorkId, html),
            ForTemplate = record_to_proplist(WorkRecord),
            {ok, Body} = work_template:render(ForTemplate),
            {Body, Req, State};
        {_, <<"edit">>} ->
            WorkRecord = ghostlight_db:get_work(WorkId, markdown),
            {AsJsonProplist} = record_to_json(WorkRecord),
            AsJson = jiffy:encode(proplists:get_value(<<"work">>, AsJsonProplist)),
            {ok, Body} = insert_work_template:render([{title, WorkRecord#work_return.work#work.title},
                                                      {editmode, AsJson}]),
            {Body, Req, State}
    end.


record_to_proplist(#work_return{
                       work=#work{
                               id = WorkId,
                               title = WorkTitle,
                               authors = Authors,
                               description = Description,
                               minutes_long = MinutesLong,
                               collaborating_org = CollabOrg
                           },
                       shows=Shows}) ->

  AuthorsProplist = [ [{author_id, AuthorId},
                       {author_name, AuthorName}] || #person{id=AuthorId, name=AuthorName} <- Authors ],
  ShowsProplist = [ [{show_id, ShowId},
                     {show_title, ShowTitle},
                     {producers, [ org_or_person_to_proplist(Producer) ||
                                   Producer <- Producers ]}
                    ] || #show{
                             id=ShowId,
                             title=ShowTitle,
                             producers=Producers
                         } <- Shows ],

  CollabOrgProplist = case CollabOrg of
                          [] ->
                              undefined;
                          [#organization{
                              id=CollabOrgId,
                              name=CollabOrgName
                            }] -> 
                              [{org_id, CollabOrgId}, {org_name, CollabOrgName}]
                      end,


  [{id, WorkId},
   {title, WorkTitle},
   {authors, AuthorsProplist},
   {description, Description},
   {minutes_long, MinutesLong},
   {collab_org, CollabOrgProplist},
   {shows, ShowsProplist}];

record_to_proplist(#work{
                      id = WorkId,
                      title=WorkTitle,
                      authors=Authors
                     }) ->
    [{<<"work_id">>, WorkId},
     {<<"title">>, WorkTitle},
     {<<"authors">>, [ ghostlight_people:record_to_proplist(Author) || Author <- Authors ]}].

org_or_person_to_proplist(#organization{id=Id, name=Name}) ->
    [{<<"type">>, <<"org">>},
     {<<"id">>, Id},
     {<<"name">>, Name}];
org_or_person_to_proplist(#person{id=Id, name=Name}) ->
    [{<<"type">>, <<"person">>},
     {<<"id">>, Id},
     {<<"name">>, Name}].

work_to_json(Req, State) ->
    WorkId = cowboy_req:binding(work_id, Req),
    case WorkId of 
        undefined ->
            Listings = ghostlight_db:get_work_listings(),
            Wrapped = {[{<<"shows">>, [ record_to_json(Work) || Work <- Listings ]}]},
            Body = jiffy:encode(Wrapped),
            {Body, Req, State};
        _ ->
            WorkRecord = ghostlight_db:get_work(WorkId, markdown),
            WorkJson = record_to_json(WorkRecord),
            Body = jiffy:encode(WorkJson),
            {Body, Req, State}
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


json_to_record({Proplist}) ->
    CollabOrg = case proplists:get_value(<<"collaborating_org">>, Proplist, null) of
                    null -> null;
                    Org -> ghostlight_org:json_to_record(Org)
                end,
    #work {
       id = proplists:get_value(<<"id">>, Proplist, null),
       title = proplists:get_value(<<"title">>, Proplist),
       authors = [ ghostlight_utils:person_or_org_json_to_record(Author)
                   || Author <- proplists:get_value(<<"authors">>, Proplist) ],
       description = proplists:get_value(<<"description">>, Proplist, null),
       minutes_long = proplists:get_value(<<"minutes_long">>, Proplist, null),
       collaborating_org = CollabOrg
    }.


post_json(Req, State) ->
    {ok, RequestBody, Req2} = cowboy_req:body(Req),
    WorkJson = jiffy:decode(RequestBody),
    WorkRecord = json_to_record(WorkJson),
    Method = cowboy_req:method(Req2),
    case {WorkRecord#work.id, Method} of
        {null, <<"POST">>} ->
            WorkId = ghostlight_db:insert_work(WorkRecord),
            Response = jiffy:encode({[{<<"id">>, list_to_binary(WorkId)}, {<<"status">>, <<"ok">>}]}),
            {true, cowboy_req:set_resp_body(Response, Req2), State};
        {_Else, <<"POST">>} ->
            Body = jiffy:encode({[{<<"error">>, <<"You may not insert a work with the field 'id'.">>}]}),
            Req3 = cowboy_req:set_resp_body(Body, Req2),
            {false, Req3, State};
        {null, <<"PUT">>} ->
            Body = jiffy:encode({[{<<"error">>, <<"You must PUT on an existing resource.">>}]}),
            Req3 = cowboy_req:set_resp_body(Body, Req2),
            {false, Req3, State};
        {_WorkId, <<"PUT">>} ->
            Success = ghostlight_db:update_work(WorkRecord),
            case Success of
                true ->
                    Response = jiffy:encode({[{<<"status">>, ok}]}),
                    {true, cowboy_req:set_resp_body(Response, Req2), State};
                false ->
                    Body = jiffy:encode({[{<<"error">>, <<"An error occurred.">>}]}),
                    Req3 = cowboy_req:set_resp_body(Body, Req2),
                    {false, Req3, State}
            end
    end.

