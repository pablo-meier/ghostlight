-module(ghostlight_show).
-export([init/2]).
-export([content_types_provided/2]).
-export([show_to_html/2,
         show_to_json/2,
         show_to_text/2]).
-export([get_fiveten/0]).

-include("apps/ghostlight/include/ghostlight_data.hrl").


init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {[
      {<<"text/html">>, show_to_html},
      {<<"application/json">>, show_to_json},
      {<<"text/plain">>, show_to_text}
     ], Req, State}.


show_to_html(Req, State) ->
    ShowInfo = [{title, <<"An Octoroon">>}],
    {ok, Body} = show_template:render(ShowInfo),
    {Body, Req, State}.

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
