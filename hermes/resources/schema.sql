-- This script creates a new cockroach database named 'helen' and also
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


-- Profile schema creation
-- Below queries are taken from hibernate logs and will have to be modified
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
create table if not exists users (userid int8 not null, email
varchar(255) UNIQUE, first_name varchar(255), last_login int8, last_name
varchar(255), name varchar(255) not null, password varchar(255) not
null, role varchar(255) not null, consortium_consortiumid int8 not
null, organization_organizationid int8 not null, primary key (userid),
foreign key (consortium_consortiumid) references consortiums, foreign
key (organization_organizationid) references organizations);

-- Blockchain entity
create table if not exists blockchains (id UUID not null unique, 
consortium_consortiumid int8 not null, ip_list varchar,
foreign key (consortium_consortiumid) references consortiums,
primary key (id));

-- keystore entity
create table if not exists keystores (address varchar(40) not null,
wallet string not null, user_userid int8, foreign key (user_userid) references users,
primary key (address));
