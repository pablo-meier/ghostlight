-- It can be a bit of a bear to add/delete things in the right order given
-- foreign key contraints -- this deletes the whole DB without dropping the
-- tables.

DELETE FROM show_dates;
DELETE FROM performance_onstage;
DELETE FROM performance_offstage;
DELETE FROM performances;
DELETE FROM authorship;
DELETE FROM works;
DELETE FROM shows;
DELETE FROM org_employees;
DELETE FROM organizations;
DELETE FROM people;
DELETE FROM users;
