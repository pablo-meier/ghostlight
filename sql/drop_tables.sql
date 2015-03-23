-- Unlike `reset_db`, this will drop the tables entirely. Useful for things
-- like migrations, playing around with data schemas, etc.

DROP TABLE show_dates CASCADE;
DROP TABLE performance_onstage CASCADE;
DROP TABLE performance_offstage CASCADE;
DROP TABLE performances CASCADE;
DROP TABLE authorship CASCADE;
DROP TABLE works CASCADE;
DROP TABLE shows CASCADE;
DROP TABLE organizations CASCADE;
DROP TABLE people CASCADE;
DROP TABLE users CASCADE;
