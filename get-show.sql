SELECT
    s.show_id,
    s.title,
    s.description_markdown,
    s.special_thanks,
    array_to_json(ARRAY(SELECT (CASE WHEN prod.person_id IS NULL
                                    THEN ('org'::person_or_org_label, prod.org_id, o.name)::person_or_org
                                    ELSE ('person'::person_or_org_label, prod.person_id, p.name)::person_or_org
                               END)
                           FROM producers prod
                           LEFT OUTER JOIN people p USING (person_id)
                           LEFT OUTER JOIN organizations o USING (org_id)
                           WHERE prod.show_id = s.show_id ORDER BY prod.listed_order DESC)) AS producers,
                  array_to_json(ARRAY(SELECT (pl.link, pl.description)::press_link FROM press_links pl WHERE show_id = s.show_id)) AS press_links,
                  array_to_json(ARRAY(SELECT (sl.link, sl.type)::external_link FROM show_links sl WHERE sl.show_id = s.show_id)) AS external_links,
                  array_to_json(ARRAY(SELECT sd.show_date from show_dates sd WHERE sd.show_id = s.show_id)) AS dates,
                  array_to_json(ARRAY(SELECT (p.person_id, p.name)::person_pair FROM people p INNER JOIN show_hosts sh USING (person_id) where sh.show_id = s.show_id)) AS hosts,
                  array_to_json(ARRAY(SELECT (perf.performance_id,
                        w.work_id, 
                        w.title, 
                        ARRAY(SELECT (CASE WHEN a.person_id IS NULL 
                                          THEN ('org'::person_or_org_label, a.org_id, o.name)::person_or_org
                                          ELSE ('person'::person_or_org_label, a.person_id, p.name)::person_or_org
                                     END)
                              FROM authorship a
                              LEFT OUTER JOIN people p USING (person_id)
                              LEFT OUTER JOIN organizations o USING (org_id)
                              WHERE a.work_id = w.work_id),
                        perf.description_markdown,
                        perf.directors_note_markdown,
                        ARRAY(SELECT (p.person_id, p.name)::person_pair
                                  FROM performance_directors pd
                                  INNER JOIN people p ON (pd.director_id = p.person_id)
                                  WHERE pd.performance_id = perf.performance_id),
                        ARRAY(SELECT ((p.person_id, p.name)::person_pair, po.role)::onstage_performance
                                  FROM performance_onstage po
                                  INNER JOIN people p ON (p.person_id = po.performer_id)
                                  WHERE po.performance_id = perf.performance_id),
                        ARRAY(SELECT (CASE WHEN po.person_id IS NULL
                                          THEN (('org'::person_or_org_label, po.org_id, o.name)::person_or_org, po.job)::offstage_performance
                                          ELSE (('person'::person_or_org_label, po.person_id, p.name)::person_or_org, po.job)::offstage_performance
                                      END)
                                FROM performance_offstage po
                                LEFT OUTER JOIN organizations o USING (org_id)
                                LEFT OUTER JOIN people p USING (person_id)
                                WHERE po.performance_id = perf.performance_id ORDER BY perf.performance_order DESC))::aggregated_performance
                       FROM works w
                       INNER JOIN performances perf USING (work_id)
                       WHERE perf.show_id = s.show_id)) AS performances
FROM shows AS s where s.show_id = $1;
