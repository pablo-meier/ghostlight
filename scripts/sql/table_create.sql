-- Toplevel file to initialize the tables. Performances and the way people
-- people can work in/for them is a little flexible, so this looks like a 
-- right mess, but that's where the challenge of the program comes in from.
--
-- A little terminology:
--   a SHOW is a showing by an organization -- e.g. Brunchaphobia, SoHo Rep's Octoroon
--   a WORK is the work being presented -- _Moment After the Meerkat_ by Charly, _An Octoroon_ by Branden Jacob-Jenkins
--   a PERFORMANCE is a unit presented at a SHOW -- 
--        _Moment After the Meerkat_ starring X, Y, and Z, directed by A, for 5/10 Still Winter.
--        _An Octoroon_ starring X, directed by Y, for _An Octoroon_


-- Table for a person, in or out of Ghostlight.
CREATE TABLE IF NOT EXISTS people (
    person_id UUID PRIMARY KEY,
    name TEXT NOT NULL,
    statement TEXT,
    photo_url TEXT,
    date_added date NOT NULL
);

-- Table for Ghostlight users. Refers to `People` for more of their details.
CREATE TABLE IF NOT EXISTS users (
    user_id UUID PRIMARY KEY,
    person_id UUID UNIQUE NOT NULL REFERENCES people(person_id),
    vanity_name TEXT UNIQUE,
    email_address TEXT UNIQUE NOT NULL,
    password_hash TEXT UNIQUE NOT NULL,
    salt TEXT NOT NULL,
    last_login timestamp with TIME ZONE NOT NULL,
    date_joined timestamp with TIME ZONE NOT NULL
);

-- Organizations that put on shows.
CREATE TABLE IF NOT EXISTS organizations (
    org_id UUID PRIMARY KEY,
    parent_org UUID,
    name text NOT NULL,
    tagline TEXT,
    description TEXT,
    vanity_name TEXT,
    date_founded date,
    visibility TEXT NOT NULL DEFAULT 'public'
);

-- Festivals are collections of shows, like Fringe, Serials, or Asking For Trouble.
-- Each show is different, but they are linked.
CREATE TABLE IF NOT EXISTS festivals (
    festival_id UUID PRIMARY KEY,
    title text NOT NULL,
    producing_org_id UUID REFERENCES organizations(org_id) NOT NULL,
    description TEXT,
    date_founded DATE
);

-- Shows, per above. If it's got a program, it's a show.
CREATE TABLE IF NOT EXISTS shows (
    show_id UUID PRIMARY KEY,
    title text NOT NULL,
    producing_org_id UUID REFERENCES organizations(org_id) NOT NULL,
    festival_id UUID REFERENCES festivals(festival_id),
    description TEXT,
    special_thanks TEXT,
    date_created TIMESTAMP WITH TIME ZONE NOT NULL
);

-- Works, per above. If you can get a script, it's a work.
CREATE TABLE IF NOT EXISTS works (
    work_id UUID PRIMARY KEY,
    title TEXT NOT NULL,
    description_src TEXT,
    description_markdown TEXT,

    -- Sometimes a work is made 'with' an org. Think You On The Moors Now with TRE, etc.
    collaborating_org_id UUID REFERENCES organizations(org_id),

    minutes_long INTEGER,
    acl TEXT NOT NULL DEFAULT 'public'
);


-- A unit of performance of a SHOW, per above. If it's got a cast, it's a performance.
CREATE TABLE IF NOT EXISTS performances (
    performance_id UUID PRIMARY KEY,
    work_id UUID REFERENCES works(work_id) NOT NULL,
    show_id UUID REFERENCES shows(show_id) NOT NULL, 
    performance_order INTEGER
);

-- Since performances can have many directors, give them a 1-many table.
CREATE TABLE IF NOT EXISTS performance_directors (
    performance_id UUID REFERENCES performances(performance_id) NOT NULL,
    director_id UUID REFERENCES people(person_id) NOT NULL
);


-- One-to-Many for people onstage during a performance.
CREATE TABLE IF NOT EXISTS performance_onstage (
    performance_id UUID REFERENCES performances(performance_id) NOT NULL,
    performer_id UUID REFERENCES people(person_id) NOT NULL,
    role TEXT DEFAULT 'Ensemble',
    -- Maybe understudies should be a new table? Given that they can change around, like performers.
    understudy_id UUID REFERENCES people(person_id),
    date_started DATE,
    date_ended DATE,
    PRIMARY KEY(performance_id, performer_id)
);


-- One-to-Many for people offstage during a performance.
CREATE TABLE IF NOT EXISTS performance_offstage (
    performance_id UUID REFERENCES performances(performance_id) NOT NULL,
    person_id UUID REFERENCES people(person_id) NOT NULL,
    job TEXT,
    date_started DATE,
    date_ended DATE,
    PRIMARY KEY(performance_id, person_id)
);

CREATE TABLE IF NOT EXISTS authorship (
    work_id UUID REFERENCES works(work_id) NOT NULL,
    person_id UUID REFERENCES people(person_id) NOT NULL,
    PRIMARY KEY(work_id, person_id)
);


CREATE TABLE IF NOT EXISTS show_dates (
   show_id UUID REFERENCES shows(show_id) NOT NULL,
   show_date TIMESTAMP WITH TIME ZONE NOT NULL,
   PRIMARY KEY(show_id, show_date)
); 


CREATE TABLE IF NOT EXISTS org_memberships (
    org_id UUID REFERENCES organizations(org_id) NOT NULL,
    user_id UUID REFERENCES users(user_id) NOT NULL,
    role TEXT NOT NULL,
    PRIMARY KEY(org_id, user_id)
);


CREATE TABLE IF NOT EXISTS org_employees (
    org_id UUID REFERENCES organizations(org_id) NOT NULL,
    person_id UUID REFERENCES people(person_id) NOT NULL,
    title TEXT,
    date_started DATE,
    date_ended DATE,
    PRIMARY KEY(org_id, person_id)
);
