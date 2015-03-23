-module(ghostlight_org).
-export([init/2,
         content_types_provided/2,
         content_types_accepted/2,
         allowed_methods/2]).
-export([org_to_html/2,
         org_to_json/2]).

-export([json_to_record/1]).

-include("apps/ghostlight/include/ghostlight_data.hrl").

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"DELETE">>],
     Req, State}.
content_types_provided(Req, State) ->
    {[
      {<<"text/html">>, org_to_html},
      {<<"application/json">>, org_to_json}
     ], Req, State}.
content_types_accepted(Req, State) ->
    {[
      {<<"application/json">>, post_json}
     ], Req, State}.

org_to_html(Req, State) ->
    OrgId = cowboy_req:binding(org_id, Req),
    case OrgId of
        <<"new">> ->
            {ok, Body} = insert_org_template:render([]),
            {Body, Req, State};
        _ ->
            OrgRecord = ghostlight_db:get_org(OrgId),
            lager:info("OrgRecord returned from DB is ~p~n", [OrgRecord]),
            ForTemplate = record_to_proplist(OrgRecord),
            lager:info("~n~nProplist is ~p~n", [ForTemplate]),
            {ok, Body} = org_template:render(ForTemplate),
            {Body, Req, State}
    end.

%% Makes the Record returned from the DB into a proplist we can feed the template.
%% Aw hell yeah Pattern Matching.
record_to_proplist(#org_return{
                     org=#organization{
                         name=Name,
                         tagline=Tagline,
                         description=Description
                     },
                     shows_produced=Shows,
                     employees=Employees}) ->
  ShowProplist = [ [{show_id, ShowId},
                    {show_title, ShowTitle},
                    {performances, [ [{work_id, WorkId}, {work_title, WorkTitle}] 
                                       || #performance{work=#work{id=WorkId, title=WorkTitle}} <- Performances ]} 
                   ] || #show{ id=ShowId,
                               title=ShowTitle,
                               performances=Performances
                             } <- Shows ],
  EmployeesProplist = [ [{person_id, PersonId},
                         {person_name, PersonName},
                         {title, Title}] || #org_employee{ title=Title,
                                                           person=#person{
                                                                     id=PersonId,
                                                                     name=PersonName
                                                                    }
                                                         } <- Employees ],


  [{name, Name},
   {tagline, Tagline},
   {description, Description},
   {shows, ShowProplist},
   {employees, EmployeesProplist}].

json_to_record({Organization}) ->
    OrgId = proplists:get_value(<<"id">>, Organization, null),
    OrgName = proplists:get_value(<<"name">>, Organization, null),
    #organization{
       id = OrgId,
       name = OrgName 
    }.

org_to_json(Req, State) ->
    {<<"{ \"status\": \"ok\" }">>, Req, State}.
