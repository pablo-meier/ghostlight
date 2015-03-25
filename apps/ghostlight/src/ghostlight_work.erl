-module(ghostlight_work).
-export([init/2,
         content_types_provided/2,
         content_types_accepted/2,
         allowed_methods/2]).
-export([work_to_html/2,
         work_to_json/2]).

-export([json_to_record/1,
         record_to_json/1]).

-include("apps/ghostlight/include/ghostlight_data.hrl").

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
                               authors = Authors
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

  [{title, WorkTitle},
   {authors, AuthorsProplist},
   {shows, ShowsProplist}].


work_to_json(_Req, _State) ->
  <<"wat">>.

record_to_json(#work{
                  id=WorkId,
                  title=WorkTitle,
                  authors=WorkAuthors
               }) ->
    ghostlight_utils:json_with_valid_values([
        {<<"work_id">>, WorkId},
        {<<"title">>, WorkTitle},
        {<<"authors">>, [ ghostlight_people:record_to_json(Author) || Author <- WorkAuthors ]}
    ]).

json_to_record({_Work}) ->
    #work{
       title = <<"Pablo!!!">>
    }.

