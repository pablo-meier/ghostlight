%% DATATYPES!
%%
%% We use records where possible, and provide defaults. It's a bit like Go where
%% we try to define a '0 type' for each record so we don't get surprising
%% 'undefined's. 

-type datetime() :: {{integer(), integer(), integer()}, {integer(), integer(), float()}}.
-type organization_parent() :: {id, binary()}.

-record(person, {
    id = <<"">>   :: binary(),
    name = <<"">> :: binary()
}).

-record(external_links, {
    website = null             :: null | binary(),
    email_address = null       :: null | binary(),
    blog = null                :: null | binary(),
    mailing_list = null        :: null | binary(),
    facebook = null            :: null | binary(),
    twitter = null             :: null | binary(),
    instagram = null           :: null | binary(),
    vimeo = null               :: null | binary(),
    youtube = null             :: null | binary()
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
    id = <<"">>                :: binary(),
    name = <<"">>              :: binary(),
    tagline = <<"">>           :: binary(),
    description = <<"">>       :: binary(),
    parent = {id, <<"">>}      :: organization_parent(),
    vanity_name = <<"">>       :: binary(),
    date_founded = {}          :: datetime(),
    external_links = null      :: null | #external_links{},
    members = null             :: null | list(#org_member{}),
    employees = null           :: null | list(#org_employee{}),
    visibility = <<"public">>  :: binary()
}).

-record(work, {
    id = <<"">>                :: binary(),
    title = <<"">>             :: binary(),
    authors = []               :: list(#person{}),
    description = null         :: null | binary(),
    collaborating_org = null   :: null | #organization{},
    minutes_long = null        :: null | integer()
}).

-record(onstage, {
    role = <<"">>      :: binary(),
    person = #person{} :: #person{}
}).

-record(offstage, {
    job = <<"">>       :: binary(),
    person = #person{} :: #person{}
}).

-record(performance, {
    work = #work{} :: #work{},
    onstage = []   :: list(#onstage{}),
    offstage = []  :: list(#offstage{}),
    directors = [] :: list(#person{})
}).

-record(show, {
    id = <<"">>             :: binary(),
    title = <<"">>          :: binary(),
    org = #organization{}   :: #organization{},
    performances = []       :: list(#performance{}),
    special_thanks = <<"">> :: binary(),
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
    id = <<"">>    :: binary(),
    name = <<"">>  :: binary(),
    authored = []  :: list(#work{}),
    directed = []  :: list(#show{}),
    onstage = []   :: list(#show{}),
    offstage = []  :: list(#show{}),
    orgs = []      :: list(#organization{})
}).

-record(org_return, {
    org = #organization{} :: #organization{},
    shows_produced = []   :: list(#show{}),
    employees = []        :: list(#org_employee{})
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

                   %% Works
                   insert_work_statement,
                   insert_authorship_statement,
                   get_work_listings,
                   get_work_meta,
                   get_work_shows,

                   %% Orgs
                   insert_org_statement,
                   insert_org_employee,
                   insert_org_member,
                   insert_org_external_link,
                   get_org_listings,
                   get_org_meta,
                   get_org_show_dates,
                   get_produced_by_org,
                   get_org_employees,

                   %% Productions
                   insert_performance_statement,
                   insert_director_statement,
                   insert_onstage_statement,
                   insert_offstage_statement,
                   insert_show_statement,
                   insert_dates_statement,
                   get_show_listings,
                   get_show_meta,
                   get_show_onstage,
                   get_show_offstage,
                   get_show_authorship,
                   get_show_directors,

                   %% People
                   insert_person_statement,
                   get_person_listings,
                   get_person_name,
                   get_person_authorship,
                   get_person_orgs,
                   get_person_onstage,
                   get_person_offstage,
                   get_person_directorships
}).

