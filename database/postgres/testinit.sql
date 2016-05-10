-- SQL code for initializing test database

-- drop tables in the correct order, avoiding constraint violations

drop table triples;
drop table strings;
drop table datatypes;
drop table nodes;
drop table namespaces;

-- create tables

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
    value bigint,
    context bigint
);

CREATE TABLE strings (
    id bigserial NOT NULL primary key,
    language bigint,
    text text NOT NULL
);

ALTER TABLE nodes OWNER TO fco;
ALTER TABLE datatypes OWNER TO fco;
ALTER TABLE namespaces OWNER TO fco;
ALTER TABLE triples OWNER TO fco;
ALTER TABLE strings OWNER TO fco;

-- insert data

INSERT INTO namespaces (iri, prefix) 
    VALUES ('http://functionalconcepts.org/system#', 'sys');
INSERT INTO namespaces (iri, prefix) 
    VALUES ('http://www.w3.org/1999/02/22-rdf-syntax-ns#', 'rdf');
INSERT INTO namespaces (iri, prefix) 
    VALUES ('http://www.w3.org/2000/01/rdf-schema#', 'rdfs');

-- indexes

CREATE INDEX idx_iri ON namespaces USING btree (iri);
CREATE INDEX idx_prefix ON namespaces USING btree (prefix);

CREATE INDEX idx_node_name ON nodes USING btree (namespace, name);

CREATE INDEX fki_datatype_node ON datatypes USING btree (node);

CREATE INDEX fki_datatype ON triples USING btree (datatype);
CREATE INDEX fki_context ON triples USING btree (context);
CREATE INDEX idx_spo ON triples USING btree (subject, predicate, datatype, value);
CREATE INDEX idx_po ON triples USING btree (predicate, datatype, value);
CREATE INDEX idx_os ON triples USING btree (datatype, value, subject);

CREATE INDEX fki_language ON strings USING btree (language);

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
ALTER TABLE ONLY triples
    ADD CONSTRAINT context FOREIGN KEY (context) REFERENCES nodes(id);

ALTER TABLE ONLY strings
    ADD CONSTRAINT language FOREIGN KEY (language) REFERENCES nodes(id);
