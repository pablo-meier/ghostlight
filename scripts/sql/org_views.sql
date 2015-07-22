-- Create a bunch of views so we can compose the Grand Old Queries readably


-- For any given show_id, returns all the works performed in that show,
-- in a parseable format of {'work': {'id': UUID, 'title': 'string'}}
CREATE VIEW performances_in_show AS
    SELECT 
        s.show_id,
        (SELECT to_json(array_agg(show_works)) AS performances
         FROM (SELECT 
                   (w.work_id, w.title)::titled_pair AS work
                   FROM works w
                   INNER JOIN performances p USING (work_id)
                   INNER JOIN shows s2 USING (show_id)
                   WHERE s2.show_id = s.show_id
                   ORDER BY p.performance_order) AS show_works)
    FROM shows s;


-- Get a show's dates as a JSON array.
CREATE VIEW show_dates_as_row AS
    SELECT
        s.show_id,
        array_to_json(ARRAY(SELECT sd.show_date FROM show_dates sd WHERE sd.show_id = s.show_id ORDER BY sd.show_date DESC)) AS show_dates
    FROM shows s;


-- Retrieve a show's opening night.
CREATE VIEW show_opening_night AS
    SELECT s.show_id,
           MIN(sd.show_date) AS opening_night
    FROM shows s
    INNER JOIN show_dates sd USING (show_id)
    GROUP BY s.show_id;


-- Get all the shows an org has produced.
CREATE VIEW org_produced_shows AS
    SELECT
        o.org_id,
        (SELECT to_json(array_agg(shows_agg))
         FROM (SELECT s.show_id AS id,
                   s.title,
                   pis.performances,
                   sdr.show_dates AS dates,
                   son.opening_night
               FROM shows s
               INNER JOIN producers USING (show_id)
               INNER JOIN performances_in_show pis USING (show_id)
               INNER JOIN show_dates_as_row sdr USING (show_id)
               INNER JOIN show_opening_night son USING (show_id)
               WHERE producers.org_id = o.org_id
               ORDER BY opening_night DESC) AS shows_agg) AS shows
    FROM organizations o;


-- Get the employees of an org.
CREATE VIEW org_employee_list AS
    SELECT
        o.org_id,
        (SELECT to_json(array_agg(emp_agg))
         FROM (SELECT json_build_object('id', p.person_id, 'name', p.name) AS person,
                      oe.title,
                      oe.description_src AS description,
                      oe.description_markdown
               FROM org_employees oe
               INNER JOIN people p USING (person_id)
               WHERE oe.org_id = o.org_id) AS emp_agg) AS employees
    FROM organizations o;


-- Get the members of an org.
CREATE VIEW org_member_list AS
    SELECT
        o.org_id,
        (SELECT to_json(array_agg(emp_agg))
         FROM (SELECT json_build_object('id', p.person_id, 'name', p.name) AS person,
                      om.description_src AS description,
                      om.description_markdown
               FROM org_members om
               INNER JOIN people p USING (person_id)
               WHERE om.org_id = o.org_id) AS emp_agg) AS members
    FROM organizations o;


-- Finally, compose an org to be a parseable JSON object.
CREATE VIEW parseable_org AS
    SELECT 
        o.org_id,
        (SELECT to_json((array_agg(org_agg))[1])
         FROM (SELECT
             org.org_id AS id,
             org.name AS name,
             org.tagline_src AS tagline,
             org.tagline_markdown,
             org.description_src AS description,
             org.description_markdown,
             array_to_json(ARRAY(SELECT (ol.link, ol.type)::external_link
                                   FROM org_links ol
                                   WHERE ol.org_id = o.org_id)) AS social,
             oel.employees,
             oml.members
         FROM
             organizations org
         LEFT JOIN org_employee_list oel USING (org_id)
         LEFT JOIN org_member_list oml USING (org_id)
         WHERE o.org_id = org.org_id) AS org_agg) AS json
    FROM organizations o;
