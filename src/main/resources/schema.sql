-- These queries are taken from hibernate logs and will have to be modified
-- if we add a new persistent entity or update existing entity.

-- Sequence used by hibernate for assigning auto generated ID
create sequence if not exists hibernate_sequence start 1 increment 1;

-- Consoritums entity
create table if not exists consortiums (consortiumid int8 not null,
consortium_name varchar(255),
consortium_type varchar(255), primary key (consortiumid));

-- Organizations entity
create table if not exists organizations (organizationid int8 not null,
organization_name varchar(255),
primary key (organizationid));

-- Users entity
create table if not exists users (userid int8 not null, email varchar(255),
first_name varchar(255), last_login timestamp, last_name varchar(255),
name varchar(255), password varchar(255), role varchar(255),
consortium_consortiumid int8 not null,
organization_organizationid int8 not null, primary key (userid),
foreign key (consortium_consortiumid) references consortiums,
foreign key (organization_organizationid) references organizations);