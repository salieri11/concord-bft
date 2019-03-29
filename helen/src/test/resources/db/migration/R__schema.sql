-- This script is a copy of the schema.sql file, edited for use with an embedded postgres DB, 
-- and flyway migration to populate the tables.  This is 

-- Profile schema creation
-- Below queries are taken from hibernate logs and will have to be modified
-- if we add a new persistent entity or update existing entity.

-- Sequence used by hibernate for assigning auto generated ID
create sequence if not exists hibernate_sequence start 1 increment 1;

-- contracts --
create sequence if not exists contract_sequence start with 1 increment 1;

create table if not exists contracts (id UUID not null, contract_id text not null, version_name text not null,
address text, sourcecode text, bytecode text, metadata text, owner text,
sequence_number integer default nextval('contract_sequence'),
blockchain_id UUID not null,
primary key (id));

-- entity tables --
create table if not exists entity (
  created_id   bigserial,
  row_key      uuid   not null,
  column_name  varchar(64)  not null,
  version      int          not null,
  body         jsonb         not null, -- 64kb; native json in 10.2?
  user_id      uuid   not null,
  user_name    varchar(64)  not null,
  created_tms  timestamp    not null  default current_timestamp,
  primary key (created_id),
  unique (row_key, version)
);

create table if not exists entity_history (
  created_id   bigserial,
  row_key      uuid   not null,
  column_name  varchar(64)  not null,
  version      int          not null,
  body         jsonb         not null, -- 64kb; native json in 10.2?
  user_id      uuid   not null,
  user_name    varchar(64)  not null,
  created_tms  timestamp    not null  default current_timestamp,
  primary key (created_id),
  unique (row_key, version)
);

-- relationships should be kept in entities' bodies. link table is a performance optimization for queries
create table if not exists link (
  from_row  uuid  not null,
  to_row    uuid  not null,
  unique (from_row, to_row)
  );


