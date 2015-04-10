

SELECT
    w.work_id,
    w.title,
    array_to_json(ARRAY(SELECT (o.org_id, o.name)::org_pair 
                        FROM organizations o
                        WHERE o.org_id = w.collaborating_org_id)) AS collaborating_org,
    array_to_json(ARRAY(SELECT (CASE WHEN a.person_id IS NULL
                                   THEN ('org'::person_or_org_label, a.org_id, o.name)::person_or_org
                                   ELSE ('person'::person_or_org_label, a.person_id, p.name)::person_or_org
                               END)
                        FROM authorship a
                        LEFT OUTER JOIN people p USING (person_id)
                        LEFT OUTER JOIN organizations o USING (org_id)
                        WHERE a.work_id = w.work_id)) AS authors,
    w.description_markdown,
    w.minutes_long,
    array_to_json(ARRAY(SELECT (s.show_id,
                                s.title,
                                ARRAY(SELECT (CASE WHEN prod.person_id IS NULL
                                                  THEN ('org'::person_or_org_label, prod.org_id, o.name)::person_or_org
                                                  ELSE ('person'::person_or_org_label, prod.person_id, p.name)::person_or_org
                                              END)
                                       FROM producers prod
                                       LEFT OUTER JOIN people p USING (person_id)
                                       LEFT OUTER JOIN organizations o USING (org_id)
                                       WHERE prod.show_id = s.show_id ORDER BY prod.listed_order DESC))::production_abbrev
                        FROM shows s 
                        INNER JOIN performances p USING (show_id)
                        WHERE p.work_id = w.work_id)) AS productions
FROM works w WHERE work_id = $1;
