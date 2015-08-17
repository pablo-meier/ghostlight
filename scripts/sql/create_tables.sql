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
    description_src TEXT,
    description_markdown TEXT,
    date_created TIMESTAMP WITH TIME ZONE NOT NULL,
    last_updated TIMESTAMP WITH TIME ZONE NOT NULL
);

-- Table for Ghostlight users. Refers to `People` for more of their details.
CREATE TABLE IF NOT EXISTS users (
    user_id UUID PRIMARY KEY,
    person_id UUID UNIQUE NOT NULL REFERENCES people(person_id),
    vanity_name CITEXT UNIQUE,
    date_created TIMESTAMP WITH TIME ZONE NOT NULL,
    last_updated TIMESTAMP WITH TIME ZONE NOT NULL
);
CREATE INDEX users_vanity_names ON users(vanity_name);

-- Organizations that put on shows.
CREATE TABLE IF NOT EXISTS organizations (
    org_id UUID PRIMARY KEY,
    name text NOT NULL,
    vanity_name CITEXT UNIQUE,
    tagline_src TEXT,
    tagline_markdown TEXT,
    description_src TEXT,
    description_markdown TEXT,
    date_founded date,
    date_created TIMESTAMP WITH TIME ZONE NOT NULL,
    last_updated TIMESTAMP WITH TIME ZONE NOT NULL
);
CREATE INDEX orgs_vanity_names ON organizations(vanity_name);


-- Shows, per above. If it's got a program, it's a show.
CREATE TABLE IF NOT EXISTS shows (
    show_id UUID PRIMARY KEY,
    vanity_name CITEXT UNIQUE,
    title text NOT NULL,
    description_src TEXT,
    description_markdown TEXT,
    special_thanks TEXT,
    date_created TIMESTAMP WITH TIME ZONE NOT NULL,
    last_updated TIMESTAMP WITH TIME ZONE NOT NULL
);
CREATE INDEX shows_vanity_names ON shows(vanity_name);

CREATE TABLE IF NOT EXISTS producers (
    show_id UUID NOT NULL REFERENCES shows(show_id),
    org_id UUID REFERENCES organizations(org_id),
    person_id UUID REFERENCES people(person_id),
    listed_order INTEGER NOT NULL,

    CONSTRAINT one_entity CHECK (org_id IS NULL != person_id IS NULL)
);

-- Works, per above. If you can get a script, it's a work.
CREATE TABLE IF NOT EXISTS works (
    work_id UUID PRIMARY KEY,
    vanity_name CITEXT UNIQUE,
    title TEXT NOT NULL,
    description_src TEXT,
    description_markdown TEXT,
    minutes_long INTEGER,
);
CREATE INDEX works_vanity_names ON works(vanity_name);


-- A unit of performance of a SHOW, per above. If it's got a cast, it's a performance.
CREATE TABLE IF NOT EXISTS performances (
    performance_id UUID PRIMARY KEY,
    work_id UUID REFERENCES works(work_id) NOT NULL,
    show_id UUID REFERENCES shows(show_id) NOT NULL, 
    directors_note_src TEXT,
    directors_note_markdown TEXT,
    description_src TEXT,
    description_markdown TEXT,
    performance_order INTEGER NOT NULL
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
    role TEXT,
    date_started DATE,
    date_ended DATE,
    PRIMARY KEY(performance_id, performer_id)
);


-- One-to-Many for people offstage during a performance.
CREATE TABLE IF NOT EXISTS performance_offstage (
    performance_id UUID REFERENCES performances(performance_id) NOT NULL,
    person_id UUID REFERENCES people(person_id),
    org_id UUID REFERENCES organizations(org_id),
    job TEXT,
    date_started DATE,
    date_ended DATE,

    CONSTRAINT one_entity CHECK (org_id IS NULL <> person_id IS NULL),
    PRIMARY KEY(performance_id, person_id, org_id)
);

CREATE TABLE IF NOT EXISTS authorship (
    work_id UUID REFERENCES works(work_id) NOT NULL,
    person_id UUID REFERENCES people(person_id),
    org_id UUID REFERENCES organizations(org_id),
    author_type authorship_type[] NOT NULL DEFAULT '{Author}'::authorship_type[],

    CONSTRAINT one_entity CHECK (org_id IS NULL != person_id IS NULL),
    PRIMARY KEY(work_id, person_id, org_id)
);

CREATE TYPE authorship_type AS ENUM (
    'Author',
    'Book',
    'Lyrics',
    'Music');

CREATE TABLE IF NOT EXISTS show_dates (
   show_id UUID REFERENCES shows(show_id) NOT NULL,
   show_date TIMESTAMP WITH TIME ZONE NOT NULL,
   PRIMARY KEY(show_id, show_date)
); 


CREATE TABLE IF NOT EXISTS org_members (
    org_id UUID REFERENCES organizations(org_id) NOT NULL,
    person_id UUID REFERENCES people(person_id) NOT NULL,
    description_src TEXT,
    description_markdown TEXT,
    date_started DATE,
    date_ended DATE,
    PRIMARY KEY(org_id, person_id)
);


CREATE TABLE IF NOT EXISTS org_employees (
    org_id UUID REFERENCES organizations(org_id) NOT NULL,
    person_id UUID REFERENCES people(person_id) NOT NULL,
    title TEXT NOT NULL,
    description_src TEXT,
    description_markdown TEXT,
    date_started DATE,
    date_ended DATE,
    PRIMARY KEY(org_id, person_id)
);

CREATE TYPE link_type AS ENUM (
    'website',
    'email',
    'facebook',
    'twitter',
    'instagram',
    'vimeo',
    'youtube',
    'blog',
    'newsletter',
    'pinterest',
    'tumblr',
    'gplus',
    'patreon',
    'newplayx');


CREATE TABLE IF NOT EXISTS org_links (
   org_id UUID REFERENCES organizations(org_id) NOT NULL,
   link TEXT NOT NULL,
   type link_type NOT NULL,
   PRIMARY KEY(org_id, type)
); 

CREATE TABLE IF NOT EXISTS people_links (
   person_id UUID REFERENCES people(person_id) NOT NULL,
   link TEXT NOT NULL,
   type link_type NOT NULL,
   PRIMARY KEY(person_id, type)
);

CREATE TABLE IF NOT EXISTS show_links (
   show_id UUID REFERENCES shows(show_id) NOT NULL,
   link TEXT NOT NULL,
   type link_type NOT NULL,
   PRIMARY KEY(show_id, type)
);

CREATE TABLE IF NOT EXISTS press_links (
   show_id UUID REFERENCES shows(show_id) NOT NULL,
   link TEXT NOT NULL,
   description TEXT NOT NULL,
   PRIMARY KEY(show_id, link)
);

CREATE TABLE IF NOT EXISTS show_hosts (
    show_id UUID REFERENCES shows(show_id) NOT NULL,
    person_id UUID REFERENCES people(person_id) NOT NULL,
    PRIMARY KEY(show_id, person_id)
);


-- These types are for the queries we do on the resources, which will often require
-- composite data.

CREATE TYPE named_pair AS (id UUID, name TEXT);
CREATE TYPE titled_pair AS (id UUID, title TEXT);

CREATE TYPE work_and_authors AS (id UUID, title TEXT, authors JSON);

CREATE TYPE external_link AS (link TEXT, type link_type);
CREATE TYPE press_link AS (link TEXT, description TEXT);

CREATE TYPE person_or_org_label AS ENUM ('person', 'org');
CREATE TYPE person_or_org AS (type person_or_org_label, id UUID, name TEXT);

CREATE TYPE onstage_performance AS ( performer person_pair, role TEXT);
CREATE TYPE offstage_performance AS ( entity person_or_org, job TEXT);

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

CREATE TYPE production_abbrev AS (
    show_id UUID,
    title TEXT,
    producers person_or_org[]
);

