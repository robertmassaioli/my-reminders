-- Atlassian Account IDs can be up to 128 characters long:
-- https://developer.atlassian.com/cloud/confluence/user-privacy-developer-guide/
ALTER TABLE reminder ADD COLUMN userAaid VARCHAR(150);