-module(ghostlight_db_utils).
-export([connect_to_postgres/0,
         markdown_or_null/1,
         fresh_uuid/0,
         null_if_unspecified/1,
         get_state/1,
         exec_batch/2,
         external_links_sql_to_record/1]).

-include("apps/ghostlight/include/ghostlight_data.hrl").


connect_to_postgres() ->
    {ok, C} = epgsql:connect("localhost", "pablo", "", [{database, "ghostlight-dev"}]),
    C.

fresh_uuid() ->
    uuid:to_string(uuid:uuid4()).

null_if_unspecified({}) -> null;
null_if_unspecified(null) -> null;
null_if_unspecified(<<"">>) -> null;
null_if_unspecified(Else) -> Else.

markdown_or_null(null) -> null;
markdown_or_null(Body) when is_binary(Body) ->
    Parsed = ghostlight_markdown:parse_markdown(Body),
    ghostlight_sanitizer:sanitize(Parsed).

get_state(Connection) ->
    Specifics = lists:foldl(fun (Fun, Accum) ->
                                    Fun(Connection, Accum)
                            end, #db_state{}, [fun ghostlight_db_show:prepare_statements/2,
                                               fun ghostlight_db_work:prepare_statements/2,
                                               fun ghostlight_db_org:prepare_statements/2,
                                               fun ghostlight_db_person:prepare_statements/2]),
    {ok, BeginStmt} = epgsql:parse(Connection, "begin_statement", "BEGIN", []),
    {ok, CommitStmt} = epgsql:parse(Connection, "commit_statement", "COMMIT", []),

    Specifics#db_state{
      connection=Connection,
      begin_statement=BeginStmt,
      commit_statement=CommitStmt
    }.


exec_batch(Batch, #db_state{connection=C,
                            commit_statement=COMMIT,
                            begin_statement=BEGIN}) ->
    AsTransaction = lists:append([ [{BEGIN, []}],
                                   Batch,
                                   [{COMMIT, []}] ]),
    Results = epgsql:execute_batch(C, AsTransaction),
    Results.

external_links_sql_to_record(Links) ->
    lists:foldl(fun({Link, Type}, Accum) ->
                        case Link of 
                            null -> Accum;
                            _ -> case Type of
                                <<"website">> -> Accum#external_links{ website=Link };
                                <<"email">> -> Accum#external_links{ email_address=Link };
                                <<"facebook">> -> Accum#external_links{ facebook=Link };
                                <<"twitter">> -> Accum#external_links{ twitter=Link };
                                <<"instagram">> -> Accum#external_links{ instagram=Link };
                                <<"vimeo">> -> Accum#external_links{ vimeo=Link };
                                <<"youtube">> -> Accum#external_links{ youtube=Link };
                                <<"blog">> -> Accum#external_links{ blog=Link };
                                <<"newsletter">> -> Accum#external_links{ mailing_list=Link }
                            end
                        end
                end, #external_links{}, Links).

