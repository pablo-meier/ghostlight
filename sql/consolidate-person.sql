-- Sometimes duplicate entries in `people` will get created if they have 
-- the same name (and we didn't link to an existing user ID; see the Rachel
-- Lin entries resulting from inserting 5/10). With all the foreign key
-- constraints you need to do the updates in a proper order otherwise it
-- goes awry.

-- BAD RACHEL LIN:  757a399a-8dde-4d6f-8f84-1e1a72364544
-- GOOD RACHEL LIN: 0a5dc7e5-b8d3-474c-af2b-d789a134c651

UPDATE authorship SET person_id = $1 WHERE person_id = $2;
UPDATE org_employee SET person_id = $1 WHERE person_id = $2;
UPDATE performance_offstage SET person_id = $1 WHERE person_id = $2;
UPDATE performance_onstage SET performer_id = $1 WHERE performer_id = $2;
UPDATE performances SET director_id = $1 WHERE director_id = $2;
UPDATE users SET person_id $1 = WHERE person_id = $2;
DELETE FROM people WHERE person_id = $2;
