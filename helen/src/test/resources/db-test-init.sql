-- Profile schema creation
-- Below queries are taken from hibernate logs and will have to be modified
-- if we add a new persistent entity or update existing entity.

-- Sequence used by hibernate for assigning auto generated ID
create sequence if not exists hibernate_sequence start 1 increment 1;

-- Consoritums entity
create table if not exists consortiums (consortiumid UUID not null,
consortium_name varchar(255),
consortium_type varchar(255), primary key (consortiumid));

-- Organizations entity
create table if not exists organizations (organizationid UUID not null,
organization_name varchar(255),
primary key (organizationid));

-- Users entity
create table if not exists users (userid UUID not null, email
varchar(255) UNIQUE, first_name varchar(255), last_login int8, last_name
varchar(255), name varchar(255) not null, password varchar(255) not
null, role varchar(255) not null, consortium_consortiumid UUID not
null, organization_organizationid UUID not null, primary key (userid),
foreign key (consortium_consortiumid) references consortiums, foreign
key (organization_organizationid) references organizations);

-- Blockchain entity
create table if not exists blockchains (id UUID not null unique, 
consortium_consortiumid UUID not null, ip_list varchar,
foreign key (consortium_consortiumid) references consortiums,
primary key (id));

	-- User Agreements
create table if not exists agreements (id int8 not null unique,
type varchar(255) not null, first_name varchar(255),
last_name varchar(255), company varchar(255), accepted_on int8,
accepted boolean not null default false,
content text not null);

-- keystore entity
create table if not exists keystores (address varchar(40) not null,
wallet string not null, user_userid UUID, foreign key (user_userid) references users,
primary key (address));
