-module(ghostlight_work_tests).
-include_lib("eunit/include/eunit.hrl").

-include("apps/ghostlight/include/ghostlight_data.hrl").



%%% JSON to record

work_fixture(File) -> ghostlight_test_utils:read_fixture(["works"] ++ [File]).

kitchen_sink() ->
    #work {
        id = <<"d96042f6-8529-4b20-915d-3e93abafc036">>,
        title= <<"or what she will">>,
        vanity_name = <<"owsw">>,
        authors = [#authorship {
                      author = #person{ id = <<"bd74206d-7bac-4d24-af99-0910e762c81a">>, name = <<"Charly Evon Simpson">> },
                      types = [written]
                   }],
        description = <<"Twins Willa and Faulkner share everything, including their parents' love of literature. At age 13, they are old souls yet still secretly believe in fantastical worlds… a storm rolls in… waves crash… FLASH… an empty playground… a man walks by… FLASH… Sometimes a moment changes everything."/utf8>>,
        minutes_long = 90
    }.

mothertongue() ->
    #work {
        title = <<"'mothertongue'">>,
        authors = [
            #authorship {
                author = #person { name = <<"Stevo Arnoczy">> },
                types = [{other, <<"Projection Design">>}]
            },
            #authorship {
                author = #person { name = <<"Trevor F. Salter">> },
                types = [choreography]
            }]
    }.


%%% JSON-TO-RECORD
kitchen_sink_test() ->
    Input = work_fixture("kitchen_sink.json"),
    Expected = kitchen_sink(),
    test_json_to_record(Input, Expected).

mothertongue_test() ->
    Input = work_fixture("mothertongue.json"),
    Expected = mothertongue(),
    test_json_to_record(Input, Expected).


test_json_to_record(Input, Expected) ->
    Result = ghostlight_work:json_to_record(jsx:decode(Input)),
    ?assertEqual(Expected, Result).


%%% RECORD-TO-JSON
base_show_deserialize_test() ->
    Input = kitchen_sink(),
    Expected = work_fixture("kitchen_sink.json"),
    test_record_to_json(Input, Expected).


test_record_to_json(Show, Expected) ->
    %% Run the input through an extra decode/encode cycle to match whatever ordering JSX
    %% uses.
    Result = jsx:prettify(jsx:encode(ghostlight_work:record_to_json(Show))),
    ExpectedEncoded = jsx:prettify(jsx:encode(jsx:decode(Expected))),
    ?assertEqual(ExpectedEncoded, Result).


%%% RECORD VALIDATION

%% Minimum viability
work_validate_id_test() ->
    ghostlight_work:validate_work(#work{id = <<"87ac576a-5645-400a-90e3-60e8f717a889">>,
                                        authors = [#authorship{
                                                        author = #person{ id = <<"481e9c01-8284-4b9f-8263-f8e3a8f0aa32">> },
                                                        types = [written]
                                                   }]
                                        }).

work_validate_name_test() ->
    ghostlight_work:validate_work(#work{title = <<"Winners lol">>,
                                        authors = [#authorship{
                                                        author = #person{ id = <<"481e9c01-8284-4b9f-8263-f8e3a8f0aa32">> },
                                                        types = [written]
                                                   }]
                                        }).


work_bad_author_person_test() ->
    WorkToTest = #work {
        id = <<"87ac576a-5645-400a-90e3-60e8f717a889">>,
        authors = [
            #authorship {
                author = #person {
                    id = <<"481e9c01-8284-4b9f-8263-f8e3a8f0aa32">>
                },
                types = [written]
            },
            #authorship {
                author = #person {
                    id = <<"beat-it-ese">>
                },
                types = [music, lyrics]
            }
        ]
    },
    ?assertException(throw, not_valid_uuid, ghostlight_work:validate_work(WorkToTest)).


work_bad_author_org_test() ->
    WorkToTest = #work {
        id = <<"87ac576a-5645-400a-90e3-60e8f717a889">>,
        authors = [
            #authorship {
                author = #person {
                    id = <<"481e9c01-8284-4b9f-8263-f8e3a8f0aa32">>
                },
                types = [book, lyrics, music]
            },
            #authorship {
                author = #organization {},
                types = [choreography]
            }
        ]
    },
    ?assertException(throw, org_missing_identifying_information, ghostlight_work:validate_work(WorkToTest)).


work_bad_vanity_name_test() ->
    WorkToTest = #work{
        title = <<"It's a Shakespeare knock-off!!!!">>,
        authors = [#authorship{ author = #person{ id = <<"481e9c01-8284-4b9f-8263-f8e3a8f0aa32">> }, types = [written] }],
        vanity_name = <<"jkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjk">>
    },
    ?assertException(throw, invalid_vanity_name_format, ghostlight_work:validate_work(WorkToTest)).
