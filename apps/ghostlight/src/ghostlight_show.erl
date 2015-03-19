-module(ghostlight_show).
-export([init/2]).
-export([content_types_provided/2,
         allowed_methods/2]).
-export([show_to_html/2,
         show_to_json/2,
         show_to_text/2]).
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
    lager:info("ShowID is ~p", [ShowId]),

    {Meta, Performances, Authorship} = ghostlight_db:get_show(ShowId),
    [{Title, OrgName, SpecialThanks, Date}] = Meta,
    
    Authors = authors_to_map(Authorship),

    ForTemplate = [{title, Title},
                   {presenting_org_name, OrgName},
                   {special_thanks, SpecialThanks},
                   {dates, [Date]},
                   {performances, performances_as_proplist(Performances, Authors)}
                  ],

    lager:info("ForTemplate is ~p", [ForTemplate]),

    {ok, Body} = show_template:render(ForTemplate),
    {Body, Req, State}.

performances_as_proplist(Performances, AuthorMap) ->
    PerformancesMap = lists:foldl(fun ({Title, _, Performer, Role}, Accum) ->
                                      PerformerProplist = [{name, Performer}, {role, Role}],
                                      case maps:get(Title, Accum, none) of
                                          none ->
                                              maps:put(Title, [PerformerProplist], Accum);
                                          Performers ->
                                              maps:put(Title, [PerformerProplist|Performers], Accum)
                                      end
                                  end, maps:new(), Performances),

    DirectorMap = lists:foldl(fun ({Title, Director, _, _}, Accum) ->
                                      case maps:get(Title, Accum, none) of
                                          none ->
                                              maps:put(Title, Director, Accum);
                                          Director ->
                                              Accum
                                      end
                                  end, maps:new(), Performances),


    Proplists = lists:map(fun (Title) ->
                              [{work, [{title, Title},
                               {authors, maps:get(Title, AuthorMap)}]},
                               {directors, [maps:get(Title, DirectorMap)]},
                               {onstage, maps:get(Title, PerformancesMap)}]
                          end, maps:keys(PerformancesMap)),
    Proplists.


%% Given a list of Authorship like the one we return from SQL, gives us a map where every key is the title
%% of a work, and the value is a list of all its authors.
authors_to_map(AuthorList) ->
    lists:foldl(fun ({Title, Author}, Accum) ->
            case maps:get(Title, Accum, none) of
                none ->
                    maps:put(Title, [Author], Accum);
                Authors ->
                    maps:put(Title, [Author|Authors], Accum)
            end
        end, maps:new(), AuthorList).

show_to_json(Req, State) ->
    Body = <<"{\"data\": \"It's a show!\"}">>,
    {Body, Req, State}.

show_to_text(Req, State) ->
    {<<"It's a show!">>, Req, State}.


get_fiveten() ->
    #show{
        title = <<"5-10: Still Winter">>,
        special_thanks = <<"The Alchemical, Ria T. DiLullo, Milica Bogetic & Paul Meier">>,
        dates = [#ghostlight_datetime {
                    time8601 = <<"2015-03-09T20:00:00">>,
                    timezone = <<"EST">>
                }],
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
