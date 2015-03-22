-module(ghostlight_show).
-export([init/2]).
-export([content_types_provided/2,
         content_types_accepted/2,
         allowed_methods/2]).
-export([show_to_html/2,
         show_to_json/2,
        
         post_json/2]).
-export([get_fiveten/0]).

-include("apps/ghostlight/include/ghostlight_data.hrl").


init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"DELETE">>],
     Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"text/html">>, show_to_html},
      {<<"application/json">>, show_to_json}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[
      {<<"application/json">>, post_json}
     ], Req, State}.


%    ShowInfo = [{title, <<"5-10 Still Winter">>},
%                {presenting_org_name, <<"SoHo Rep">>},
%                {special_thanks, <<"This guy!">>},
%                {dates, [<<"Saturday, March 10 2015, 8:00">>]},
%                {performances, [
%                                [{work, [{title, <<"Literally Stupid">>}, {authors, [<<"Adin Lenahan">>]}]},
%                                 {directors, [<<"Karen Eilbacher">>]},
%                                 {onstage, [[{name, <<"Olivia Jampol">>}, {role, <<"Erica">>}],
%                                            [{name, <<"Ryan Dreyer">>}, {role, <<"Jerome">>}],
%                                            [{name, <<"Shane Hall">>}, {role, <<"Caleb">>}]]}]
%                               ]}],

show_to_html(Req, State) ->
    ShowId = cowboy_req:binding(show_id, Req),
    case ShowId of
        <<"new">> ->
            {ok, Body} = insert_show_template:render([]),
            {Body, Req, State};
        _ ->
            ShowRecord = ghostlight_db:get_show(ShowId),
            ForTemplate = record_to_proplist(ShowRecord),
            {ok, Body} = show_template:render(ForTemplate),
            {Body, Req, State}
    end.

record_to_proplist(ShowRecord) ->
    [{title, ShowRecord#show.title},
     {presenting_org_name, ShowRecord#show.org#organization.name},
     {special_thanks, ShowRecord#show.special_thanks},
     {dates, ShowRecord#show.dates},
     {performances, records_to_proplist(ShowRecord#show.performances)}
    ].


records_to_proplist(Performances) ->
    lists:map(fun(#performance{work=Work, onstage=Onstage, directors=Directors}) ->
                  #work{title=Title, authors=AuthorsTuples} = Work,
                  DirectorNames = extract_names(Directors),
                  AuthorNames = extract_names(AuthorsTuples),
                  OnstageList = onstage_to_proplist(Onstage),
                  [
                   {work, [{title, Title}, {authors, AuthorNames}]},
                   {directors, DirectorNames},
                   {onstage, OnstageList}
                  ]
              end, Performances).

extract_names(ListOfPeople) ->
    lists:map(fun({name, Name}) -> Name end, ListOfPeople).

onstage_to_proplist(ListOnstage) ->
    lists:map(fun(#onstage{role=Role, person=Person}) ->
                  [{role, Role}, Person]
              end, ListOnstage).



show_to_json(Req, State) ->
    ShowId = cowboy_req:binding(show_id, Req),
    _ShowRecord = ghostlight_db:get_show(ShowId),
%    Body = jiffy:encode({CorrectedPropList}),
    {<<"{ \"status\": \"ok\" }">>, Req, State}.


post_json(Req, State) ->
    % upload the body, return yes or no
    {ok, RequestBody, Req2} = cowboy_req:body(Req),
    ShowRecord = show_json_to_record(RequestBody),
    lager:info("~nShowRecord is ~p", [ShowRecord]),
    Response = ghostlight_db:insert_show(ShowRecord),
    lager:info("~nResponse from DB server is ~p~n", [Response]),

    {true, cowboy_req:set_resp_body(<<"ok">>, Req2), State}.


show_json_to_record(JsonInput) ->
    {Decoded} = jiffy:decode(JsonInput),
    lager:info("~nWe have an Erlang object: ~p~n", [{Decoded}]),

    Title = proplists:get_value(<<"title">>, Decoded),
    SpecialThanks = proplists:get_value(<<"special_thanks">>, Decoded),
    Dates = lists:map(fun iso8601:parse/1, proplists:get_value(<<"dates">>, Decoded)),
    Org = organization_json_to_record(proplists:get_value(<<"org">>, Decoded)),
    Performances = lists:map(fun performance_json_to_record/1, proplists:get_value(<<"performances">>, Decoded)),

    #show{
        title = Title,
        special_thanks = SpecialThanks,
        dates = Dates,
        org = Org,
        performances = Performances
    }.


performance_json_to_record({Proplist}) ->
    Work = work_json_to_record(proplists:get_value(<<"work">>, Proplist)),
    Onstage = lists:map(fun onstage_json_to_record/1, proplists:get_value(<<"onstage">>, Proplist, [])),
    Offstage = lists:map(fun offstage_json_to_record/1, proplists:get_value(<<"offstage">>, Proplist, [])),
    Directors = lists:map(fun ghostlight_people:json_to_record/1, proplists:get_value(<<"directors">>, Proplist)),
    #performance{
       work = Work,
       directors = Directors,
       onstage = Onstage,
       offstage = Offstage
    }.


work_json_to_record({Proplist}) ->
    #work {
       title = proplists:get_value(<<"title">>, Proplist),
       authors = lists:map(fun ghostlight_people:json_to_record/1, proplists:get_value(<<"authors">>, Proplist))
    }.


organization_json_to_record({Org}) ->
    Name = proplists:get_value(<<"name">>, Org),
    Tagline = proplists:get_value(<<"tagline">>, Org),
    Description = proplists:get_value(<<"description">>, Org),
    #organization{
       name = Name,
       tagline = Tagline,
       description = Description
    }.


onstage_json_to_record({Onstage}) ->
    Performer = ghostlight_people:json_to_record(proplists:get_value(<<"performer">>, Onstage)),
    Role = proplists:get_value(<<"role">>, Onstage),
    #onstage{
      person = Performer,
      role = Role
    }.

offstage_json_to_record({Offstage}) ->
    Contributor = ghostlight_people:json_to_record(proplists:get_value(<<"contributor">>, Offstage)),
    Job = proplists:get_value(<<"job">>, Offstage),
    #offstage{
      person = Contributor,
      job = Job 
    }.


get_fiveten() ->
    #show{
        title = <<"5-10: Still Winter">>,
        special_thanks = <<"The Alchemical, Ria T. DiLullo, Milica Bogetic & Paul Meier">>,
        dates = [{{2015, 3, 9}, {20, 0, 0.0}}],
        org = #organization{
                  name = <<"Plumage">>,
                  tagline = <<"A Co-Lab between blackbird delight and Marrow's Edge">>
              },
        performances = [
                        #performance{
                            work = #work {
                                      title = <<"Literally Stupid">>,
                                      authors = [{name, <<"Adin Lenahan">>}]
                                   },
                            onstage = [
                                       #onstage{
                                          role = <<"Erica">>,
                                          person = {name, <<"Olivia Jampol">>}
                                      },
                                       #onstage{
                                          role = <<"Jerome">>,
                                          person = {name, <<"Ryan Dreyer">>}
                                      },
                                       #onstage{
                                          role = <<"Caleb">>,
                                          person = {name, <<"Shane Hall">>}
                                      }
                                      ],
                            directors = [{name, <<"Karen Eilbacher">>}]
                        },
                        #performance{
                            work = #work {
                                      title = <<"Tights">>,
                                      authors = [{name, <<"Lily Padilla">>}]
                                   },
                            onstage = [
                                       #onstage{
                                          role = <<"Manny">>,
                                          person = {name, <<"Christopher Curry">>}
                                      },
                                       #onstage{
                                          role = <<"Mr. Reaves">>,
                                          person = {name, <<"Jim Shankman">>}
                                      }
                                      ],
                            directors = [{name, <<"Avriel Hillman">>}]
                        },
                        #performance{
                            work = #work {
                                      title = <<"Let The Cat Out of the Bag">>,
                                      authors = [{name, <<"Joy Tomasko">>}]
                                   },
                            onstage = [
                                       #onstage{
                                          role = <<"Doorman">>,
                                          person = {name, <<"Ethan Nguyen">>}
                                      },
                                       #onstage{
                                          role = <<"Voice">>,
                                          person = {name, <<"Megan Hopp">>}
                                      }
                                      ],
                            directors = [{name, <<"Osh Ghanimah">>}]
                        },
                        #performance{
                            work = #work {
                                      title = <<"The Moment After the Meerkat">>,
                                      authors = [{name, <<"Charly Evon Simpson">>}]
                                   },
                            onstage = [
                                       #onstage{
                                          role = <<"Adam">>,
                                          person = {name, <<"Jimmy Dailey">>}
                                      },
                                       #onstage{
                                          role = <<"Bridget">>,
                                          person = {name, <<"Rachel Lin">>}
                                      }
                                      ],
                            directors = [{name, <<"Michael Raine">>}]
                        },
                        #performance{
                            work = #work {
                                      title = <<"The Louboutin">>,
                                      authors = [{name, <<"Daria Miyeko Marinelli">>}]
                                   },
                            onstage = [
                                       #onstage{
                                          role = <<"Lil">>,
                                          person = {name, <<"Irene Rivera">>}
                                      },
                                       #onstage{
                                          role = <<"Phene">>,
                                          person = {name, <<"Rachel Lin">>}
                                      }
                                      ],
                            directors = [{name, <<"Eric Powell Holm">>}]
                        },
                        #performance{
                            work = #work {
                                      title = <<"Meditations in an Avalanche">>,
                                      authors = [{name, <<"Elijah Guo">>}]
                                   },
                            onstage = [
                                       #onstage{
                                          role = <<"Jessica">>,
                                          person = {name, <<"Sohina Sidhu">>}
                                      },
                                       #onstage{
                                          role = <<"Claude">>,
                                          person = {name, <<"Matt Stango">>}
                                      }
                                      ],
                            directors = [{name, <<"Lilleth Glimcher">>}]
                       }]
      }.
