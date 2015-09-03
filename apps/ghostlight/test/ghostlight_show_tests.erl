-module(ghostlight_show_tests).
-include_lib("eunit/include/eunit.hrl").

-include("apps/ghostlight/include/ghostlight_data.hrl").

show_fixture(File) -> ghostlight_test_utils:read_fixture(["shows"] ++ [File]).



%% Shows have a much more thorough set of minimums. This is a base case that
%% we can add to to test individual properties.
base_show() ->
    #show {
        id = <<"d7962932-b04c-4572-a904-5fe1b94187f4">>,
        title = <<"Citizen Sarah Kane: The Musical!!">>,
        producers = [
            #person{
                id = <<"ca962932-b04c-4572-a904-5fe1b94187ff">>
            },
            #organization {
                id = <<"adaf2932-b04c-4572-a904-5fe1b6418721">>
            }
        ],
        performances = [
            #performance {
               work = #work {
                   title = <<"Leech Beach">>,
                   authors = [#authorship {
                       author = #person { name = <<"Alucard 👻"/utf8>> },
                       types = [written]
                   }]
               },
               onstage = [
                   #onstage{
                       person = #person {
                           id = <<"09cc9ae2-b04c-4572-a201-59381ccc3891">>
                       },
                       role = <<"Bellerephon">>
                   },
                   #onstage{
                       person = #person {
                           id = <<"09cc9ae2-b04c-4572-a201-59381ccc3891">>
                       }
                   }
               ],
               offstage = [
                   #offstage {
                       contributor = #person { id = <<"12239ae2-b04c-4572-a201-59381ccc3891">> },
                       jobs = [<<"Stage Hand">>, <<"Stage Armpit">>]
                   },
                   #offstage {
                       contributor = #organization { name = <<"Fled Nanders">> },
                       jobs = [<<"Sounding Designer">>]
                   }],
               directors = [
                  #person {
                    id = <<"ca3f2932-b04c-4572-a904-5fe1b6418721">>
                  },
                  #person {
                    id = <<"99a22018-b04c-4572-a904-ba917492aec1">>
                  }
               ],
               directors_note = <<"Started from the bottom, still at the bottom.">>,
               description = <<"The most daring play to have eggrolls since _Apocalypse, WOW!_">>
            }
        ],
        description = <<"**There were nights of endless pleasure.**">>,
        special_thanks = <<"Me, and nobody else">>,
        hosts = [],
        press_links = [],
        external_links = null,
        dates = [{{2015,3,9},{20,0,0}}]
    }.


%%% JSON-TO-RECORD
kitchen_sink_test() ->
    Input = show_fixture("kitchen_sink.json"),
    Expected = base_show(),
    test_json_to_record(Input, Expected).


test_json_to_record(Input, Expected) ->
    Result = ghostlight_show:json_to_record(jsx:decode(Input)),
    ?assertEqual(Expected, Result).


%%% RECORD-TO-JSON
base_show_deserialize_test() ->
    Input = base_show(),
    Expected = show_fixture("kitchen_sink.json"),
    test_record_to_json(Input, Expected).


test_record_to_json(Show, Expected) ->
    %% Run the input through an extra decode/encode cycle to match whatever ordering JSX
    %% uses.
    Result = jsx:prettify(jsx:encode(ghostlight_show:record_to_json(Show))),
    ExpectedEncoded = jsx:prettify(jsx:encode(jsx:decode(Expected))),
    ?assertEqual(ExpectedEncoded, Result).


%%% RECORD VALIDATION
%% Minimum viability
validate_show_test() ->
    Show = base_show(),
    Show = ghostlight_show:validate_show(Show).

validate_show_id_no_title_test() ->
    Show = base_show(),
    NoId = Show#show{ id = null },
    NoId = ghostlight_show:validate_show(NoId).

validate_show_title_no_id_test() ->
    Show = base_show(),
    NoTitle = Show#show{ title = null },
    NoTitle = ghostlight_show:validate_show(NoTitle).

validate_show_requires_id_or_title_test() ->
    Show = base_show(),
    NoIdentifiers = Show#show{ title = null, id = null },
    ?assertException(throw, show_missing_identifying_information, ghostlight_show:validate_show(NoIdentifiers)).

validate_show_requires_dates_test() ->
    Show = base_show(),
    NoIdentifiers = Show#show{ dates = [] },
    ?assertException(throw, {list_not_correct_size, _}, ghostlight_show:validate_show(NoIdentifiers)).

validate_show_requires_performances_test() ->
    Show = base_show(),
    NoPerformances = Show#show{ performances = [] },
    ?assertException(throw, {list_not_correct_size, _}, ghostlight_show:validate_show(NoPerformances)).

validate_show_requires_producers_test() ->
    Show = base_show(),
    NoProducers = Show#show{ producers = [] },
    ?assertException(throw, {list_not_correct_size, _}, ghostlight_show:validate_show(NoProducers)).

validate_show_invalid_producer_test() ->
    Show = base_show(),
    NoProducers = Show#show{ producers = [#person { id = <<"lol que">> }] },
    ?assertException(throw, not_valid_uuid, ghostlight_show:validate_show(NoProducers)).

validate_show_invalid_performance_test() ->
    Show = base_show(),
    NoProducers = Show#show{ performances = [#performance{}] },
    ?assertException(throw, work_missing_identifying_information, ghostlight_show:validate_show(NoProducers)).

