    -- Works Authored
    -- Works under collaboration
    -- Offstage jobs

SELECT
    o.org_id,
    o.name,
    o.tagline,
    o.description_markdown,
    array_to_json(ARRAY(SELECT (ol.link, ol.type)::external_link
                            FROM org_links ol
                            WHERE ol.org_id = o.org_id)) AS links,
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
               WHERE producers.org_id = o.org_id) AS prod
    ) AS shows_produced,
    (
        SELECT to_json(array_agg(emp))
        FROM (SELECT oe.org_id, p.person_id, p.name, oe.title, oe.description_markdown AS description
              FROM org_employees oe
              INNER JOIN people p USING (person_id)) AS emp 
        WHERE emp.org_id = o.org_id
    ) AS employees, 
    (
        SELECT to_json(array_agg(mem))
        FROM (SELECT om.org_id, p.person_id, p.name, om.description_markdown AS description
              FROM org_members om
              INNER JOIN people p USING (person_id)) AS mem
        WHERE mem.org_id = o.org_id
    ) AS members
FROM organizations o
WHERE o.org_id = $1;
