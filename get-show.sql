-- Hokay... what do we want?

-- Show ID,
-- Show Title
-- Description
-- Special Thanks

-- List of producers

-- List of performances
-- * work ID
-- * work title
-- * list of work authors
-- * * either ("org", orgId, orgName) or ("person", personId, personName)
-- * Description_markdown
-- * directors_note_markdown
-- * list of directors
-- * * DirectorId
-- * * DirectorName
-- * list of onstage
-- * * PersonId
-- * * PersonName
-- * * Role
-- * list of offstage

-- ALMOST THERE!
-- WE DID IT BOYS
-- some sanity in all aggregations so Erlang can read (json functions?)
-- Cleanup of the types.



-- Onstage here we go lol
-- SELECT perf.performance_id, ARRAY(SELECT ((p.person_id, p.name)::person_pair, po.role)::onstage_performance FROM performance_onstage po INNER JOIN people p ON (p.person_id = po.performer_id) WHERE po.performance_id = perf.performance_id) FROM performances perf WHERE perf.performance_id = 'd079edca-9a1a-41ca-901b-0552a52c0955';

-- Performance query!
--
-- SELECT perf.performance_id,
--        w.work_id, 
--        w.title, 
--        ARRAY(SELECT (CASE WHEN a.person_id IS NULL 
--                          THEN ('org'::person_or_org_label, a.org_id, o.name)::person_or_org
--                          ELSE ('person'::person_or_org_label, a.person_id, p.name)::person_or_org
--                     END)
--              FROM authorship a
--              LEFT OUTER JOIN people p USING (person_id)
--              LEFT OUTER JOIN organizations o USING (org_id)
--              WHERE a.work_id = w.work_id) AS authors,
--        perf.description_markdown AS description,
--        perf.directors_note_markdown AS directors_note,
--        ARRAY(SELECT (p.person_id, p.name)::person_pair
--                  FROM performance_directors pd
--                  INNER JOIN people p ON (pd.director_id = p.person_id)
--                  WHERE pd.performance_id = perf.performance_id) AS directors,
--        ARRAY(SELECT ((p.person_id, p.name)::person_pair, po.role)::onstage_performance
--                  FROM performance_onstage po
--                  INNER JOIN people p ON (p.person_id = po.performer_id)
--                  WHERE po.performance_id = perf.performance_id) AS onstage,
--        ARRAY(SELECT (CASE WHEN po.person_id IS NULL
--                          THEN (('org'::person_or_org_label, po.org_id, o.name)::person_or_org, po.job)::offstage_performance
--                          ELSE (('person'::person_or_org_label, po.person_id, p.name)::person_or_org, po.job)::offstage_performance
--                      END)
--                FROM performance_offstage po
--                LEFT OUTER JOIN organizations o USING (org_id)
--                LEFT OUTER JOIN people p USING (person_id)
--                WHERE po.performance_id = perf.performance_id) as offstage
-- FROM works w
-- INNER JOIN performances perf USING (work_id)
-- WHERE perf.performance_id = 'd079edca-9a1a-41ca-901b-0552a52c0955';

-- Aggregated over a show!

-- Lists of hosts
-- * PersonId
-- * PersonName
-- ARRAY(SELECT (p.person_id, p.name)::person_pair FROM people p INNER JOIN show_hosts sh USING (person_id) where sh.show_id = '70429dee-9591-41b1-9f2d-f85d1e567c33') AS hosts,

-- List of Press Links
-- * Link
-- * Description
-- SELECT ARRAY(SELECT (pl.link, pl.description)::press_link FROM press_links pl WHERE show_id = '2fda92d2-1ab3-43df-ac65-b5292b371649')

-- List of External Links
-- * Link
-- * Type
-- SELECT ARRAY(SELECT (sl.link, sl.type)::external_link FROM show_links sl WHERE show_id = '2fda92d2-1ab3-43df-ac65-b5292b371649');

-- List of Performance dates
-- SELECT ARRAY(SELECT show_date) from show_dates WHERE show_id = '2fda92d2-1ab3-43df-ac65-b5292b371649';

CREATE TYPE person_pair AS (id UUID, name TEXT);
CREATE TYPE external_link AS (link TEXT, type link_type);
CREATE TYPE press_link AS (link TEXT, description TEXT);
CREATE TYPE aggregated_performance AS (
    performance_id UUID,
    work_id UUID,
    work_title TEXT,
    authors person_or_org[],
    description TEXT,
    directors_note TEXT,
    directors person_pair[],
    onstage onstage_performance[],
    offstage offstage_performance[]
);
CREATE TYPE person_or_org_label AS ENUM (
    'person',
    'org');

CREATE TYPE onstage_performance AS (
    performer person_pair,
    role TEXT
);

CREATE TYPE person_or_org AS (
    type person_or_org_label,
    id UUID,
    name TEXT
);

CREATE TYPE offstage_performance AS (
    entity person_or_org,
    job TEXT
);


SELECT
    s.show_id,
    s.title,
    s.description_markdown,
    s.special_thanks,
    ARRAY(SELECT (CASE WHEN prod.person_id IS NULL
                      THEN ('org'::person_or_org_label, prod.org_id, o.name)::person_or_org
                      ELSE ('person'::person_or_org_label, prod.person_id, p.name)::person_or_org
                 END)
             FROM producers prod
             LEFT OUTER JOIN people p USING (person_id)
             LEFT OUTER JOIN organizations o USING (org_id)
             WHERE prod.show_id = s.show_id ORDER BY prod.listed_order ASC) AS producers,
    ARRAY(SELECT (pl.link, pl.description)::press_link FROM press_links pl WHERE show_id = s.show_id) AS press_links,
    ARRAY(SELECT (sl.link, sl.type)::external_link FROM show_links sl WHERE sl.show_id = s.show_id) AS external_links,
    ARRAY(SELECT sd.show_date from show_dates sd WHERE sd.show_id = s.show_id) AS dates,
    ARRAY(SELECT (p.person_id, p.name)::person_pair FROM people p INNER JOIN show_hosts sh USING (person_id) where sh.show_id = s.show_id) AS hosts,
    ARRAY(SELECT (perf.performance_id,
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
                  WHERE po.performance_id = perf.performance_id ORDER BY perf.performance_order ASC))::aggregated_performance
         FROM works w
         INNER JOIN performances perf USING (work_id)
         WHERE perf.show_id = s.show_id) AS performances
FROM shows AS s where s.show_id = '88e1ac64-ea3d-482a-964b-1605dee9e7c6';
