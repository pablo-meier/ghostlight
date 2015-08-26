
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
