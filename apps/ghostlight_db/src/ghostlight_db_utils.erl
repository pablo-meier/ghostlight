-module(ghostlight_db_utils).
-export([connect_to_postgres/0,
         markdown_or_null/1,
         fresh_uuid/0,
         null_if_unspecified/1,
         get_state/1,
         parse_person_or_org/1,
         exec_batch/2,
         external_links_sql_to_record/1,
         external_links_inserts/3]).

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


parse_person_or_org({Entity}) ->
    Id = proplists:get_value(<<"id">>, Entity, null),
    Name = proplists:get_value(<<"name">>, Entity, null),
    case proplists:get_value(<<"type">>, Entity, null) of
        <<"org">> ->
            #organization { id = Id, name = Name };
        <<"person">> ->
            #person{ id = Id, name = Name }
    end.


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
                                <<"pinterest">> -> Accum#external_links{ pinterest=Link };
                                <<"tumblr">> -> Accum#external_links{ tumblr=Link };
                                <<"gplus">> -> Accum#external_links{ gplus=Link };
                                <<"newsletter">> -> Accum#external_links{ mailing_list=Link };
                                <<"patreon">> -> Accum#external_links{ patreon=Link };
                                <<"newplayx">> -> Accum#external_links{ newplayx=Link }
                            end
                        end
                end, #external_links{}, Links).


external_links_inserts(OrgId,
                       Stmt,
                       #external_links{ 
                          website=Website, 
                          email_address=Email, 
                          blog=Blog, 
                          mailing_list=MailingList,
                          facebook=Facebook, 
                          twitter=Twitter, 
                          instagram=Instagram,
                          vimeo=Vimeo,
                          youtube=YouTube,
                          pinterest=Pinterest,
                          tumblr=Tumblr,
                          gplus=GPlus,
                          patreon=Patreon,
                          newplayx=NewPlayX
                         }) ->
    WebsiteI = null_or_link_insert(OrgId, Website, <<"website">>, Stmt),
    EmailI = null_or_link_insert(OrgId, Email, <<"email">>, Stmt),
    BlogI = null_or_link_insert(OrgId, Blog, <<"blog">>, Stmt),
    MailingListI = null_or_link_insert(OrgId, MailingList, <<"newsletter">>, Stmt),
    FacebookI = null_or_link_insert(OrgId, Facebook, <<"facebook">>, Stmt),
    TwitterI = null_or_link_insert(OrgId, Twitter, <<"twitter">>, Stmt),
    InstagramI = null_or_link_insert(OrgId, Instagram, <<"instagram">>, Stmt),
    VimeoI = null_or_link_insert(OrgId, Vimeo, <<"vimeo">>, Stmt),
    YouTubeI = null_or_link_insert(OrgId, YouTube, <<"youtube">>, Stmt),
    PinterestI = null_or_link_insert(OrgId, Pinterest, <<"pinterest">>, Stmt),
    TumblrI = null_or_link_insert(OrgId, Tumblr, <<"tumblr">>, Stmt),
    GPlusI = null_or_link_insert(OrgId, GPlus, <<"gplus">>, Stmt),
    PatreonI = null_or_link_insert(OrgId, Patreon, <<"patreon">>, Stmt),
    NewPlayXI = null_or_link_insert(OrgId, NewPlayX, <<"newplayx">>, Stmt),
    lists:filter(fun(X) -> X =/= null end, [WebsiteI,
                                            EmailI,
                                            BlogI,
                                            MailingListI,
                                            FacebookI,
                                            TwitterI,
                                            InstagramI,
                                            VimeoI,
                                            YouTubeI,
                                            PinterestI,
                                            TumblrI,
                                            GPlusI,
                                            PatreonI,
                                            NewPlayXI
                                           ]).

null_or_link_insert(OrgId, Link, Type, Stmt) ->
    case Link of
        null -> null;
        _ -> {Stmt, [OrgId, Link, Type]}
    end.


