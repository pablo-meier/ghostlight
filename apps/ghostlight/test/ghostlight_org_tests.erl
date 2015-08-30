-module(ghostlight_org_tests).
-include_lib("eunit/include/eunit.hrl").

-include("apps/ghostlight/include/ghostlight_data.hrl").

org_fixture(File) -> ghostlight_test_utils:read_fixture(["orgs"] ++ [File]).

kitchen_sink() ->
    #organization {
        id = <<"481e9c01-8284-4b9f-8263-f8e3a8f0aa32">>,
        name = <<"PIEHOLE">>,
        tagline = <<"Your face is made for **PIE.**">>,
        description = <<"The length of time I've loved Chipotle has outlasted all my romantic relationships combined.">>,
        members = [#org_member{
                      person = #person {
                                  id = <<"2d7029c7-d911-460d-914d-4eb2189b38fa">>
                               },
                      description = <<"Pop lock and drop it.">>
                     },
                   #org_member{
                      person = #person {
                                  name = <<"Poopface McGee">>
                               }
                     }],
        employees = [#org_employee{
                      person = #person {
                                  id = <<"87ac576a-5645-400a-90e3-60e8f717a889">>
                               },
                      title = <<"Crunch Muncher">>,
                      description = <<"_Never gonna give you up_, never gonna let you down.">>
                     },
                     #org_employee{
                      person = #person {
                                  name = <<"Arvo McLarbo">>
                               },
                      title = <<"Back Cracker">>
                     }],
        external_links = #external_links {
          website = <<"http://www.pieholed.com/">>,
          email_address = <<"pieholed@gmail.com">>,
          facebook = <<"https://www.facebook.com/pages/Piehole/163999200321547">>,
          instagram = <<"http://instagram.com/pieholed">>,
          mailing_list = <<"http://eepurl.com/Lxxbb">>,
          blog = <<"http://www.pieholed.com/notebook">>
        }
      }.

%%% JSON-TO-RECORD
kitchen_sink_test() -> 
    Input = org_fixture("kitchen_sink.json"),
    Expected = kitchen_sink(),
    test_json_to_record(Input, Expected).

test_json_to_record(Input, Expected) ->
    Result = ghostlight_org:json_to_record(jsx:decode(Input)),
    ?assertEqual(Expected, Result).


%%% RECORD VALIDATION

%% Minimum viability
org_validate_id_test() ->
    ghostlight_org:validate_org(#organization{id = <<"87ac576a-5645-400a-90e3-60e8f717a889">>}).
org_validate_name_test() ->
    ghostlight_org:validate_org(#organization{name = <<"Muppet Babies">>}).


org_validate_throws_no_identification_test() ->
   ?assertException(throw, org_missing_identifying_information, ghostlight_org:validate_org(#organization{})).

org_validate_id_bad_test() ->
    ?assertException(throw, not_valid_uuid, ghostlight_org:validate_org(#organization{id = <<"lol not an ID">>})).

org_validate_bad_employee_test() ->
    OrgToTest = #organization{
        name = <<"A Narrow Defeat">>,
        employees = [#org_employee{
            person=#person{
                 id = <<"lol">>
            }
        }]
    },
    ?assertException(throw, not_valid_uuid, ghostlight_org:validate_org(OrgToTest)).


org_validate_bad_member_test() ->
    OrgToTest = #organization{
        name = <<"Bilcher's Cove">>,
        members = [#org_member{
            person=#person{
                id = <<"lol">>
            }
        }]
    },
    ?assertException(throw, not_valid_uuid, ghostlight_org:validate_org(OrgToTest)).


org_bad_external_links_test() ->
    OrgToTest = #organization{
        name = <<"test">>,
        external_links = #external_links{
            facebook = <<"http://morepaul.com">>
        }
    },
    ?assertException(throw, {external_error, [facebook]}, ghostlight_org:validate_org(OrgToTest)).

org_bad_vanity_name_test() ->
    OrgToTest = #organization{
        name = <<"test">>,
        vanity_name = <<"IAmAVeryLongVanityNameThisKindOfThingReallyShouldntStandIThink">>
    },
    ?assertException(throw, invalid_vanity_name_format, ghostlight_org:validate_org(OrgToTest)).
