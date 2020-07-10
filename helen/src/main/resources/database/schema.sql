-- This script creates a new database named 'helen' and also
-- creates a new admin user named 'helen_admin' for that database. As of
-- now contracts management services and profile(user) management services
-- need this database.
-- The table creation part for profile(user) managment services is done
-- in this script (because those services are developed using hibernate and
-- so we don't have to directly deal with SQL quries). However, the
-- contract management services were developed earlier and hence they
-- directly deal with SQL queries using JDBC. Hence, contract management
-- table creation is not done here

-- Create a helen database
CREATE DATABASE IF NOT EXISTS helen;

-- Allow helen_admin all access to helen database
GRANT ALL ON DATABASE helen TO helen_admin;

-- switch to helen database
use helen;

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



