-- tables

CREATE TABLE namespaces (
    id serial NOT NULL primary key,
    iri text NOT NULL,
    prefix text
);

CREATE TABLE nodes (
    id bigserial NOT NULL primary key,
    namespace bigint,
    name text
);

CREATE TABLE datatypes (
    id serial NOT NULL primary key,
    dtname text NOT NULL,
    tablename text,
    node bigint
);

CREATE TABLE triples (
    id bigserial NOT NULL primary key,
    subject bigint NOT NULL,
    predicate bigint NOT NULL,
    datatype integer NOT NULL,
    value bigint
);

CREATE TABLE texts (
    id bigserial NOT NULL primary key,
    language bigint,
    text text NOT NULL
);

ALTER TABLE nodes OWNER TO fco;
ALTER TABLE datatypes OWNER TO fco;
ALTER TABLE namespaces OWNER TO fco;
ALTER TABLE triples OWNER TO fco;
ALTER TABLE texts OWNER TO fco;

-- data

INSERT INTO namespaces (iri, prefix) 
    VALUES ('http://functionalconcepts.org/fco-common#', 'fco');
INSERT INTO namespaces (iri, prefix) 
    VALUES ('http://www.w3.org/1999/02/22-rdf-syntax-ns#', 'rdf');
INSERT INTO namespaces (iri, prefix) 
    VALUES ('http://www.w3.org/2000/01/rdf-schema#', 'rdfs');

INSERT INTO nodes (namespace, name) VALUES (1, 'Node');    -- 1
INSERT INTO nodes (namespace, name) VALUES (2, 'type');    -- 2
INSERT INTO nodes (namespace, name) VALUES (2, 'Property');-- 3
INSERT INTO nodes (namespace, name) VALUES (2, 'Class');   -- 4
INSERT INTO nodes (namespace, name) VALUES (1, 'Datatype');-- 5

INSERT INTO datatypes (dtname, tablename) VALUES ('node', 'nodes'); -- 1
INSERT INTO datatypes (dtname, tablename) VALUES ('string', 'texts'); -- 2
INSERT INTO datatypes (dtname) VALUES ('int');                      -- 3

INSERT INTO triples (subject, predicate, datatype, value) VALUES (1, 2, 1, 5);
INSERT INTO triples (subject, predicate, datatype, value) VALUES (2, 2, 1, 3);
INSERT INTO triples (subject, predicate, datatype, value) VALUES (3, 2, 1, 4);
INSERT INTO triples (subject, predicate, datatype, value) VALUES (4, 2, 1, 4);
INSERT INTO triples (subject, predicate, datatype, value) VALUES (5, 2, 1, 4);

-- indexes

CREATE INDEX idx_iri ON namespaces USING btree (iri);
CREATE INDEX idx_prefix ON namespaces USING btree (prefix);

CREATE INDEX idx_node_name ON nodes USING btree (namespace, name);

CREATE INDEX fki_datatype_node ON datatypes USING btree (node);

CREATE INDEX fki_datatype ON triples USING btree (datatype);
CREATE INDEX idx_spo ON triples USING btree (subject, predicate, datatype, value);
CREATE INDEX idx_po ON triples USING btree (predicate, datatype, value);
CREATE INDEX idx_os ON triples USING btree (datatype, value, subject);

CREATE INDEX fki_language ON texts USING btree (language);

-- foreign key constraints

ALTER TABLE ONLY nodes
    ADD CONSTRAINT namespace FOREIGN KEY (namespace) REFERENCES namespaces(id);

ALTER TABLE ONLY datatypes
    ADD CONSTRAINT node FOREIGN KEY (node) REFERENCES nodes(id);

ALTER TABLE ONLY triples
    ADD CONSTRAINT subject FOREIGN KEY (subject) REFERENCES nodes(id);
ALTER TABLE ONLY triples
    ADD CONSTRAINT predicate FOREIGN KEY (predicate) REFERENCES nodes(id);
ALTER TABLE ONLY triples
    ADD CONSTRAINT datatype FOREIGN KEY (datatype) REFERENCES datatypes(id);

ALTER TABLE ONLY texts
    ADD CONSTRAINT language FOREIGN KEY (language) REFERENCES nodes(id);

