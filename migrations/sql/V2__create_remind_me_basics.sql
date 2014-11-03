CREATE TABLE ping 
   ( id SERIAL PRIMARY KEY
   , tenantId              INTEGER not null references tenant(id) ON DELETE CASCADE
   , issueId               INTEGER not null
   , originalIssueKey      VARCHAR(255) not null
   , originalIssueSummary  TEXT not null
   , issueKey              VARCHAR(255) not null
   , issueSummary          TEXT not null
   , userKey               VARCHAR(255) not null
   , userEmail             VARCHAR(512) not null
   , message               TEXT
   , date                  TIMESTAMP WITH TIME ZONE
   );

CREATE INDEX ping_tenant_id_idx ON ping (tenantId);
CREATE INDEX expiry_date_idx ON ping (date);

CREATE TABLE purged_tenant
   ( id        SERIAL PRIMARY KEY
   , baseUrl   VARCHAR(512) not null
   , purgeDate TIMESTAMP WITH TIME ZONE
   );

CREATE INDEX purge_date_idx ON purged_tenant (purgeDate);
