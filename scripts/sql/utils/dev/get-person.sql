-- Name
-- Authors
-- * Work ID, Work Title
-- Directed
-- * Show link, Producers
-- Onstage
-- * Show link, Producers
-- Offstage
-- * Show link, Producers
-- Links
-- Org Employees
-- Org Member
-- Producer
-- Host/Emcee

SELECT
    p.name,
    p.description_markdown AS description,
    array_to_json(ARRAY(SELECT (w.work_id, w.title)::work_pair
                        FROM works w 
                        INNER JOIN authorship a USING (work_id)
                        where a.person_id = p.person_id)) AS authorships,
    -- Director
    (
        SELECT to_json(array_agg(directed))
        FROM (SELECT s.show_id,
                     s.title,
                     array_to_json(ARRAY(SELECT (w.work_id, w.title)::work_pair
                                                FROM works w
                                                INNER JOIN performances perf USING (work_id)
                                                INNER JOIN performance_directors pd USING (performance_id)
                                                WHERE perf.show_id = s.show_id
                                                AND pd.director_id = p.person_id
                                                ORDER BY perf.performance_order)) AS works,
                     array_to_json(ARRAY(SELECT (CASE WHEN prod.person_id IS NULL
                                                     THEN ('org'::person_or_org_label, prod.org_id, o.name)::person_or_org
                                                     ELSE ('person'::person_or_org_label, prod.person_id, p.name)::person_or_org
                                                END)
                                            FROM producers prod
                                            LEFT OUTER JOIN people p USING (person_id)
                                            LEFT OUTER JOIN organizations o USING (org_id)
                                            WHERE prod.show_id = s.show_id ORDER BY prod.listed_order DESC)) AS producers
               FROM shows s
               INNER JOIN performances perf USING (show_id)
               INNER JOIN performance_directors pd USING (performance_id)
               WHERE pd.director_id = p.person_id) AS directed
    ) AS directorships,
    -- Onstage
    (
        SELECT to_json(array_agg(onstaged))
        FROM (SELECT s.show_id,
                     s.title,
                     (SELECT array_agg(collected)
                      FROM (SELECT w.work_id, w.title, po.role
                                   FROM works w
                                   INNER JOIN performances perf USING (work_id)
                                   WHERE perf.show_id = s.show_id
                                   AND perf.performance_id = po.performance_id
                                   AND po.performer_id = p.person_id
                                   ORDER BY perf.performance_order) AS collected) AS roles,
                     array_to_json(ARRAY(SELECT (CASE WHEN prod.person_id IS NULL
                                                     THEN ('org'::person_or_org_label, prod.org_id, o.name)::person_or_org
                                                     ELSE ('person'::person_or_org_label, prod.person_id, p.name)::person_or_org
                                                END)
                                            FROM producers prod
                                            LEFT OUTER JOIN people p USING (person_id)
                                            LEFT OUTER JOIN organizations o USING (org_id)
                                            WHERE prod.show_id = s.show_id ORDER BY prod.listed_order DESC)) AS producers
               FROM shows s
               INNER JOIN performances perf USING (show_id)
               INNER JOIN performance_onstage po USING (performance_id)
               WHERE po.performer_id = p.person_id) AS onstaged
    ) AS onstage,
    -- Offstage
    (
        SELECT to_json(array_agg(offstaged))
        FROM (SELECT s.show_id,
                     s.title,
                     (SELECT array_agg(collected)
                      FROM (SELECT w.work_id, w.title, po.job
                                   FROM works w
                                   INNER JOIN performances perf USING (work_id)
                                   WHERE perf.show_id = s.show_id
                                   AND perf.performance_id = po.performance_id
                                   AND po.person_id = p.person_id
                                   ORDER BY perf.performance_order) AS collected) AS jobs,
                     array_to_json(ARRAY(SELECT (CASE WHEN prod.person_id IS NULL
                                                     THEN ('org'::person_or_org_label, prod.org_id, o.name)::person_or_org
                                                     ELSE ('person'::person_or_org_label, prod.person_id, p.name)::person_or_org
                                                END)
                                            FROM producers prod
                                            LEFT OUTER JOIN people p USING (person_id)
                                            LEFT OUTER JOIN organizations o USING (org_id)
                                            WHERE prod.show_id = s.show_id ORDER BY prod.listed_order DESC)) AS producers
               FROM shows s
               INNER JOIN performances perf USING (show_id)
               INNER JOIN performance_offstage po USING (performance_id)
               WHERE po.person_id = p.person_id) AS offstaged
    ) AS offstage,
    -- Links
    array_to_json(ARRAY(SELECT (pl.link, pl.type)::external_link
                        FROM people_links pl
                        WHERE pl.person_id = p.person_id)) AS links,
    -- Employee
    (
        SELECT to_json(array_agg(emp))
        FROM (SELECT o.org_id, o.name, oe.title
              FROM organizations o
              INNER JOIN org_employees oe USING (org_id)
              WHERE oe.person_id = p.person_id) AS emp
    ) AS employee,
    -- Member 
    (
        SELECT to_json(array_agg(mem))
        FROM (SELECT o.org_id, o.name
              FROM organizations o
              INNER JOIN org_members om USING (org_id)
              WHERE om.person_id = p.person_id) AS mem
    ) AS member,
    -- Producer
    (
        SELECT to_json(array_agg(prod))
        FROM (SELECT s.show_id,
                     s.title,
                     array_to_json(ARRAY(SELECT (w.work_id, w.title)::work_pair
                                                FROM works w
                                                INNER JOIN performances p USING (work_id)
                                                WHERE p.show_id = s.show_id)) AS works
               FROM shows s
               INNER JOIN producers USING (show_id)
               WHERE producers.person_id = p.person_id) AS prod
    ) AS shows_produced
FROM people p
WHERE p.person_id = 'add131f7-3c72-41ba-95ed-ce5d62744de9';
