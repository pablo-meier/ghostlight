-- Unlike `reset_db`, this will drop the tables entirely. Useful for things
-- like migrations, playing around with data schemas, etc.

DROP TABLE performance_onstage CASCADE;
DROP TABLE performance_offstage CASCADE;
DROP TABLE performance_directors CASCADE;
DROP TABLE performances CASCADE;
DROP TABLE authorship CASCADE;
DROP TABLE works CASCADE;
DROP TABLE show_dates CASCADE;
DROP TABLE press_links CASCADE;
DROP TABLE show_links CASCADE;
DROP TABLE show_hosts CASCADE;
DROP TABLE producers CASCADE;
DROP TABLE shows CASCADE;
DROP TABLE org_links CASCADE;
DROP TABLE org_employees CASCADE;
DROP TABLE org_members CASCADE;
DROP TABLE organizations CASCADE;
DROP TABLE people_links CASCADE;
DROP TABLE users CASCADE;
DROP TABLE people CASCADE;

DROP TYPE external_link CASCADE;
DROP TYPE press_link CASCADE;
DROP TYPE aggregated_performance CASCADE;
DROP TYPE production_abbrev CASCADE;
DROP TYPE onstage_performance CASCADE;
DROP TYPE offstage_performance CASCADE;
DROP TYPE authorship_agg CASCASE;
DROP TYPE work_and_authors CASCADE;
DROP TYPE person_or_org CASCADE;
DROP TYPE person_or_org_label CASCADE;
DROP TYPE authorship_type CASCADE;
DROP TYPE titled_pair CASCADE;
DROP TYPE named_pair CASCADE;

DROP TYPE link_type CASCADE;
DROP EXTENSION citext;
