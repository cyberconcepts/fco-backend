--
-- create tables and sequences
--

-- nodes

CREATE TABLE nodes (
    id bigint NOT NULL,
    baseiri bigint,
    iri text,
    context bigint
);

ALTER TABLE nodes OWNER TO fco;

CREATE SEQUENCE nodes_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER TABLE nodes_id_seq OWNER TO fco;
ALTER SEQUENCE nodes_id_seq OWNED BY nodes.id;

-- datatypes

CREATE TABLE datatypes (
    id integer NOT NULL,
    dtname text NOT NULL,
    tablename text,
    node bigint
);

ALTER TABLE datatypes OWNER TO fco;

CREATE SEQUENCE datatypes_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER TABLE datatypes_id_seq OWNER TO fco;
ALTER SEQUENCE datatypes_id_seq OWNED BY datatypes.id;

-- iris

CREATE TABLE iris (
    id integer NOT NULL,
    iri text NOT NULL,
    baseiri text,
    prefix text
);

ALTER TABLE iris OWNER TO fco;

CREATE SEQUENCE iris_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER TABLE iris_id_seq OWNER TO fco;
ALTER SEQUENCE iris_id_seq OWNED BY iris.id;

-- triples

CREATE TABLE triples (
    id bigint NOT NULL,
    subject bigint NOT NULL,
    predicate bigint NOT NULL,
    datatype integer NOT NULL,
    value bigint,
    context bigint
);


ALTER TABLE triples OWNER TO fco;

CREATE SEQUENCE triples_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER TABLE triples_id_seq OWNER TO fco;
ALTER SEQUENCE triples_id_seq OWNED BY triples.id;

-- strings

CREATE TABLE strings (
    id bigint NOT NULL,
    language bigint,
    text text NOT NULL
);

ALTER TABLE strings OWNER TO fco;

CREATE SEQUENCE strings_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER TABLE strings_id_seq OWNER TO fco;
ALTER SEQUENCE strings_id_seq OWNED BY strings.id;

--
-- activate sequences
--

ALTER TABLE ONLY nodes ALTER COLUMN id SET DEFAULT nextval('nodes_id_seq'::regclass);
ALTER TABLE ONLY datatypes ALTER COLUMN id SET DEFAULT nextval('datatypes_id_seq'::regclass);
ALTER TABLE ONLY iris ALTER COLUMN id SET DEFAULT nextval('iris_id_seq'::regclass);
ALTER TABLE ONLY triples ALTER COLUMN id SET DEFAULT nextval('triples_id_seq'::regclass);
ALTER TABLE ONLY strings ALTER COLUMN id SET DEFAULT nextval('strings_id_seq'::regclass);

--
-- data
--

INSERT INTO iris (id, iri, baseiri, prefix) VALUES (1, 'http://functionalconcepts.org/system#', NULL, 'sys');
INSERT INTO iris (id, iri, baseiri, prefix) VALUES (3, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#', NULL, 'rdf');
INSERT INTO iris (id, iri, baseiri, prefix) VALUES (4, 'http://www.w3.org/2000/01/rdf-schema#', NULL, 'rdfs');
SELECT pg_catalog.setval('iris_id_seq', 4, true);

INSERT INTO nodes (id, baseiri, iri, context) VALUES (1, 1, 'node', NULL);
INSERT INTO nodes (id, baseiri, iri, context) VALUES (3, 1, 'predicate', NULL);
INSERT INTO nodes (id, baseiri, iri, context) VALUES (2, 3, 'type', NULL);
INSERT INTO nodes (id, baseiri, iri, context) VALUES (4, 3, 'Class', NULL);
INSERT INTO nodes (id, baseiri, iri, context) VALUES (5, 1, 'datatype', NULL);
SELECT pg_catalog.setval('nodes_id_seq', 5, true);

INSERT INTO datatypes (id, dtname, tablename) VALUES (1, 'node', 'nodes');
INSERT INTO datatypes (id, dtname, tablename) VALUES (2, 'string', 'strings');
SELECT pg_catalog.setval('datatypes_id_seq', 2, true);

INSERT INTO triples (id, subject, predicate, datatype, value, context) VALUES (3, 1, 2, 1, 5, NULL);
INSERT INTO triples (id, subject, predicate, datatype, value, context) VALUES (1, 2, 2, 1, 3, NULL);
INSERT INTO triples (id, subject, predicate, datatype, value, context) VALUES (2, 3, 2, 1, 4, NULL);
INSERT INTO triples (id, subject, predicate, datatype, value, context) VALUES (4, 4, 2, 1, 4, NULL);
INSERT INTO triples (id, subject, predicate, datatype, value, context) VALUES (5, 5, 2, 1, 4, NULL);
SELECT pg_catalog.setval('triples_id_seq', 5, true);

SELECT pg_catalog.setval('strings_id_seq', 1, false);

--
-- primary key constraints
--

ALTER TABLE ONLY nodes
    ADD CONSTRAINT nodes_pkey PRIMARY KEY (id);

ALTER TABLE ONLY datatypes
    ADD CONSTRAINT datatypes_pkey PRIMARY KEY (id);

ALTER TABLE ONLY iris
    ADD CONSTRAINT id PRIMARY KEY (id);

ALTER TABLE ONLY triples
    ADD CONSTRAINT triples_pkey PRIMARY KEY (id);

ALTER TABLE ONLY strings
    ADD CONSTRAINT strings_pkey PRIMARY KEY (id);

--
-- indexes
--

CREATE INDEX idx_iri ON iris USING btree (iri);
CREATE INDEX idx_prefix ON iris USING btree (prefix);
CREATE INDEX idx_baseiri ON iris USING btree (baseiri);

CREATE INDEX fki_node_baseiri ON nodes USING btree (baseiri);
CREATE INDEX fki_node_context ON nodes USING btree (context);
CREATE INDEX idx_node_iri ON nodes USING btree (iri);

CREATE INDEX fki_datatype_node ON datatypes USING btree (node);

CREATE INDEX fki_datatype ON triples USING btree (datatype);
CREATE INDEX fki_context ON triples USING btree (context);
CREATE INDEX idx_spo ON triples USING btree (subject, predicate, datatype, value);
CREATE INDEX idx_po ON triples USING btree (predicate, datatype, value);
CREATE INDEX idx_os ON triples USING btree (datatype, value, subject);

CREATE INDEX fki_language ON strings USING btree (language);

--
-- foreign key constraints
--

ALTER TABLE ONLY nodes
    ADD CONSTRAINT baseiri FOREIGN KEY (baseiri) REFERENCES iris(id);
ALTER TABLE ONLY nodes
    ADD CONSTRAINT context FOREIGN KEY (context) REFERENCES nodes(id);

ALTER TABLE ONLY datatypes
    ADD CONSTRAINT node FOREIGN KEY (node) REFERENCES nodes(id);

ALTER TABLE ONLY triples
    ADD CONSTRAINT subject FOREIGN KEY (subject) REFERENCES nodes(id);
ALTER TABLE ONLY triples
    ADD CONSTRAINT predicate FOREIGN KEY (predicate) REFERENCES nodes(id);
ALTER TABLE ONLY triples
    ADD CONSTRAINT datatype FOREIGN KEY (datatype) REFERENCES datatypes(id);
ALTER TABLE ONLY triples
    ADD CONSTRAINT context FOREIGN KEY (context) REFERENCES nodes(id);

ALTER TABLE ONLY strings
    ADD CONSTRAINT language FOREIGN KEY (language) REFERENCES nodes(id);

