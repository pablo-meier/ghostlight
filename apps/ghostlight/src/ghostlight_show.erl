-module(ghostlight_show).
-export([init/2]).
-export([content_types_provided/2,
         content_types_accepted/2,
         allowed_methods/2]).
-export([show_to_html/2,
         show_to_json/2,
         show_to_text/2,
        
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
      {<<"application/json">>, show_to_json},
      {<<"text/plain">>, show_to_text}
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
    ShowRecord = ghostlight_db:get_show(ShowId),
    Proplist = record_to_proplist(ShowRecord),

    %% replace the dates -- erlydtl requires Erlang datetypes, jiffy chokes.
    DateList = proplists:get_value(dates, Proplist),
    CorrectedDates = lists:map(fun(D) -> iso8601:format(remove_float(D)) end, DateList),
    lager:info("DateList was ~p, now it is ~p", [DateList, CorrectedDates]),
    CorrectedPropList = [ {dates, CorrectedDates} | proplists:delete(dates, Proplist)],

    lager:info("  Encoding: ~p", [{CorrectedPropList}]),
    Body = jiffy:encode({CorrectedPropList}),
    {Body, Req, State}.


post_json(Req, State) ->
    % upload the body, return yes or no
    {ok, RequestBody, _Req2} = cowboy_req:body(Req),
    lager:info("~n~nOH HEY IT'S A POST! BODY: ~p~n", [RequestBody]),

    Decoded = jiffy:decode(RequestBody),
    lager:info("~nWe have an Erlang object: ~p~n", [Decoded]),
    {true, cowboy_req:set_resp_body(<<"ok">>, Req), State}.


remove_float({{Y,M,D},{H,Min,Sec}}) -> {{Y,M,D},{H,Min,trunc(Sec)}}.

show_to_text(Req, State) ->
    {<<"It's a show!">>, Req, State}.


%% {
%%   title: text,
%%   special_thanks: text,
%%   dates: [iso8601],
%%   org: {
%%     name: text,
%%     tagline: text
%%   },
%%   performances: [
%%     {
%%       work: {
%%         title: text,
%%         authors: [text]
%%       },
%%       director: {
%%         name: text
%%       }
%%       onstage: [
%%         {
%%           name: text,
%%           role: text
%%         },
%%         ...
%%       ]
%%     },
%%     ...
%%   ],

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
