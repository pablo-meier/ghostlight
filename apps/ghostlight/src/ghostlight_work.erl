-module(ghostlight_work).
-export([init/2,
         content_types_provided/2,
         content_types_accepted/2,
         allowed_methods/2]).
-export([work_to_html/2,
         work_to_json/2,
         
         post_json/2]).

-export([json_to_record/1,
         record_to_json/1]).

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
content_types_provided(Req, State) ->
    {[
      {<<"text/html">>, work_to_html},
      {<<"application/json">>, work_to_json}
     ], Req, State}.
content_types_accepted(Req, State) ->
    {[
      {<<"application/json">>, post_json}
     ], Req, State}.

work_to_html(Req, State) ->
    WorkId = cowboy_req:binding(work_id, Req),
    case WorkId of
        undefined ->
            Listings = ghostlight_db:get_work_listings(),
            ForTemplate = [{<<"works">>, [ record_to_proplist(Work) || Work <- Listings ]}],
            {ok, Body} = work_listing_template:render(ForTemplate),
            {Body, Req, State};
        <<"new">> ->
            {ok, Body} = insert_work_template:render([]),
            {Body, Req, State};
        _ ->
            WorkRecord = ghostlight_db:get_work(WorkId),
            ForTemplate = record_to_proplist(WorkRecord),
            {ok, Body} = work_template:render(ForTemplate),
            {Body, Req, State}
    end.


record_to_proplist(#work_return{
                       work=#work{
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
                     {org_id, OrgId},
                     {org_name, OrgName}] || #show{
                                                id=ShowId,
                                                title=ShowTitle,
                                                org=#organization{
                                                       id=OrgId,
                                                       name=OrgName
                                                      }
                                               } <- Shows ],
  CollabOrgProplist = case CollabOrg of
                          null ->
                              undefined;
                          #organization{
                              id=CollabOrgId,
                              name=CollabOrgName
                            } -> 
                              [{org_id, CollabOrgId}, {org_name, CollabOrgName}]
                      end,


  [{title, WorkTitle},
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


work_to_json(Req, State) ->
    WorkId = cowboy_req:binding(work_id, Req),
    case WorkId of 
        undefined ->
            Listings = ghostlight_db:get_work_listings(),
            Wrapped = {[{<<"shows">>, [ record_to_json(Work) || Work <- Listings ]}]},
            Body = jiffy:encode(Wrapped),
            {Body, Req, State};
        _ ->
            WorkRecord = ghostlight_db:get_work(WorkId),
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
        {<<"work_id">>, WorkId},
        {<<"title">>, WorkTitle},
        {<<"description">>, Description},
        {<<"minutes_long">>, MinutesLong},
        {<<"authors">>, [ ghostlight_people:record_to_json(Author) || Author <- WorkAuthors ]}
    ]);

record_to_json(#work_return{
                  work=Work,
                  shows=Shows
               }) ->
    ghostlight_utils:json_with_valid_values([
        {<<"work">>, record_to_json(Work)},
        {<<"shows">>, [ ghostlight_show:record_to_json(Show) || Show <- Shows ]}
    ]).


post_json(Req, State) ->
    % upload the body, return yes or no
    {ok, RequestBody, Req2} = cowboy_req:body(Req),
    WorkJson = jiffy:decode(RequestBody),
    WorkRecord = json_to_record(WorkJson),
    WorkId = ghostlight_db:insert_work(WorkRecord),
    Response = jiffy:encode({[{<<"id">>, list_to_binary(WorkId)}, {<<"status">>, <<"ok">>}]}),
    {true, cowboy_req:set_resp_body(Response, Req2), State}.

json_to_record({Proplist}) ->
    CollabOrg = case proplists:get_value(<<"collaborating_org">>, Proplist, null) of
                    null -> null;
                    Org -> ghostlight_org:json_to_record(Org)
                end,
    #work {
       title = proplists:get_value(<<"title">>, Proplist),
       authors = lists:map(fun ghostlight_people:json_to_record/1, proplists:get_value(<<"authors">>, Proplist)),
       description = proplists:get_value(<<"description">>, Proplist, null),
       minutes_long = proplists:get_value(<<"minutes_long">>, Proplist, null),
       collaborating_org = CollabOrg
    }.
