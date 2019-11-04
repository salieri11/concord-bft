-- This script is a copy of the schema.sql file, edited for use with an embedded postgres DB, 
-- and flyway migration to populate the tables.  This is 

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

--session management table
CREATE TABLE SPRING_SESSION (
	PRIMARY_ID CHAR(36) NOT NULL,
	SESSION_ID CHAR(36) NOT NULL,
	CREATION_TIME BIGINT NOT NULL,
	LAST_ACCESS_TIME BIGINT NOT NULL,
	MAX_INACTIVE_INTERVAL INT NOT NULL,
	EXPIRY_TIME BIGINT NOT NULL,
	PRINCIPAL_NAME VARCHAR(100),
	CONSTRAINT SPRING_SESSION_PK PRIMARY KEY (PRIMARY_ID)
);

CREATE UNIQUE INDEX SPRING_SESSION_IX1 ON SPRING_SESSION (SESSION_ID);
CREATE INDEX SPRING_SESSION_IX2 ON SPRING_SESSION (EXPIRY_TIME);
CREATE INDEX SPRING_SESSION_IX3 ON SPRING_SESSION (PRINCIPAL_NAME);

CREATE TABLE SPRING_SESSION_ATTRIBUTES (
	SESSION_PRIMARY_ID CHAR(36) NOT NULL,
	ATTRIBUTE_NAME VARCHAR(200) NOT NULL,
	ATTRIBUTE_BYTES BYTEA NOT NULL,
	CONSTRAINT SPRING_SESSION_ATTRIBUTES_PK PRIMARY KEY (SESSION_PRIMARY_ID, ATTRIBUTE_NAME),
	CONSTRAINT SPRING_SESSION_ATTRIBUTES_FK FOREIGN KEY (SESSION_PRIMARY_ID) REFERENCES SPRING_SESSION(PRIMARY_ID) ON DELETE CASCADE
);