%% DATATYPES!
%%
%% We use records where possible, and provide defaults. It's a bit like Go where
%% we try to define a '0 type' for each record so we don't get surprising
%% 'undefined's. 

-type datetime() :: {{integer(), integer(), integer()}, {integer(), integer(), float()}}.
-type organization_parent() :: {id, binary()}.

-record(external_links, {
    website = null             :: null | binary(),
    email_address = null       :: null | binary(),
    blog = null                :: null | binary(),
    mailing_list = null        :: null | binary(),
    facebook = null            :: null | binary(),
    twitter = null             :: null | binary(),
    instagram = null           :: null | binary(),
    vimeo = null               :: null | binary(),
    youtube = null             :: null | binary(),
    pinterest = null           :: null | binary(),
    tumblr = null              :: null | binary(),
    gplus = null               :: null | binary(),
    patreon = null             :: null | binary(),
    newplayx = null            :: null | binary()
}).

-record(press_link, {
    link = null         :: null | binary(),
    description = null :: null | binary()
}).

-record(person, {
    id = <<"">>           :: binary(),
    name = <<"">>         :: binary(),
    external_links = null :: null | #external_links{},
    description = null    :: null | binary()
}).

-record(org_member, {
    member             :: #person{},
    description = null :: null | binary()
}).

-record(org_employee, {
    person             :: #person{},
    title = null       :: null | binary(),
    description = null :: null | binary()
}).

-record(organization, {
    id = <<"">>                        :: binary(),
    name = <<"">>                      :: binary(),
    tagline = <<"">>                   :: binary(),
    description = null                 :: binary(),
    parent = {id, <<"">>}              :: organization_parent(),
    vanity_name = <<"">>               :: binary(),
    date_founded = {}                  :: datetime(),
    external_links = #external_links{} :: #external_links{},
    members = []                       :: list(#org_member{}),
    employees = []                     :: list(#org_employee{}),
    visibility = <<"public">>          :: binary()
}).

-record(work, {
    id = <<"">>                :: binary(),
    title = <<"">>             :: binary(),
    authors = []               :: list(#person{}) | list(#organization{}),
    description = null         :: null | binary(),
    collaborating_org = null   :: null | #organization{},
    minutes_long = null        :: null | integer()
}).

-record(onstage, {
    role = <<"">>      :: binary(),
    person = #person{} :: #person{}
}).

-record(offstage, {
    job = <<"">>            :: binary(),
    contributor = #person{} :: #organization{} | #person{}
}).

-record(performance, {
    id = null               :: null | binary(),
    work = #work{}          :: #work{},
    onstage = []            :: list(#onstage{}),
    offstage = []           :: list(#offstage{}),
    directors = []          :: list(#person{}),
    directors_note = null   :: null | binary(),
    description = null      :: null | binary()
}).

-record(show, {
    id = null               :: binary(),
    title = <<"">>          :: binary(),
    producers = []          :: list(#person{} | #organization{}),
    performances = []       :: list(#performance{}),
    special_thanks = null   :: null | binary(),
    description = null      :: null | binary(),
    hosts = []              :: list(#person{}),
    press_links = []        :: list(#press_link{}),
    external_links = null   :: null | #external_links{},
    dates = []              :: list(datetime())
}).


-record(org_work, {
  org_id = <<"">>   :: binary(),
  org_name = <<"">> :: binary(),
  title = <<"">>    :: binary()
}).

%% Tentative about this -- should we be defining records per-call? I dislike the granularity
%% of the above records for returning, say all the contributions for a person, since they'll
%% touch so many things.

-record(person_return, {
    person=#person{}      :: #person{},
    authored = []         :: list(#work{}),
    directed = []         :: list(#show{}),
    onstage = []          :: list(#show{}),
    offstage = []         :: list(#show{}),
    orgs_employee = []    :: list(#org_work{}),
    orgs_member = []      :: list(#organization{}),
    shows_produced = []   :: list(#show{})
}).

-record(org_return, {
    org = #organization{} :: #organization{},
    shows_produced = []   :: list(#show{})
}).

-record(work_return, {
    work = #work{}   :: #work{},
    shows = []       :: list(#show{})
}).


%% Fuck epgsql and the false promise of prepared statements. Everything is tied to the goddamn
%% connection and now everyone in the db applications has to share this monolithic shit and
%% break my refactor.
-record(db_state, {connection,
                   begin_statement,
                   commit_statement,
                   rollback_statement,

                   %% Works
                   insert_work_statement,
                   insert_authorship_statement,
                   get_work_listings,
                   get_work_statement,
                   update_work_statement,
                   delete_authors_statement,

                   %% Orgs
                   insert_org_statement,
                   insert_org_employee,
                   insert_org_member,
                   insert_org_external_link,
                   get_org_listings,
                   get_org_statement,
                   update_org_statement,
                   delete_org_employees,
                   delete_org_members,
                   delete_org_links,

                   %% Productions
                   insert_performance_statement,
                   insert_director_statement,
                   insert_onstage_statement,
                   insert_offstage_statement,
                   insert_show_statement,
                   insert_dates_statement,
                   insert_hosts_statement,
                   insert_links_statement,
                   insert_presslinks_statement,
                   insert_producer_statement,

                   get_show_listings,
                   get_show_statement,

                   %% People
                   insert_person_statement,
                   insert_person_links_statement,
                   get_person_listings,
                   get_person_statement,
                   update_person_statement,
                   delete_person_links_statement
}).
