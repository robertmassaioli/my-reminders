﻿-- Database: pingme

DROP TABLE IF EXISTS tenant;

CREATE TABLE tenant (
     id  SERIAL PRIMARY KEY
  ,  key VARCHAR(255) unique not null
  ,  publicKey TEXT not null
  ,  sharedSecret VARCHAR(512) null
  ,  baseUrl VARCHAR(512) unique not null
  ,  productType VARCHAR(50) not null
);

DROP INDEX IF EXISTS tenant_idx;

CREATE INDEX tenant_idx ON tenant (sharedSecret);

DROP TABLE IF EXISTS ping;

CREATE TABLE ping 
   ( id SERIAL PRIMARY KEY
   , tenantId   INTEGER not null
   , issueId    INTEGER not null
   , issueKey   VARCHAR(255) not null
   , issueSummary TEXT not null
   , userKey    VARCHAR(255) not null
   , userEmail  VARCHAR(512) not null
   , message    TEXT
   , date       TIMESTAMP WITH TIME ZONE
   );
