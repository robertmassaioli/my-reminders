TENANT_COUNT=100
REMINDERS_PER_TENANT=1000

PSQL="bash run-psql.bash"

CREATE_FILE="create-test-data.generated.sql"

if [ -f "$CREATE_FILE" ]
then
   rm "$CREATE_FILE"
fi

for tenantId in `seq 1 $TENANT_COUNT`
do
   KEY=`uuid`
   PUBLIC_KEY="public-key-$KEY"
   SHARED_SECRET="shared-secret-$KEY"
   BASE_URL="https://$tenantId.atlassian.com"
   PRODUCT_TYPE="JIRA"
   echo "INSERT INTO tenant (id, key, publickey, sharedsecret, baseurl, producttype) VALUES ($tenantId, 'jira:$KEY', '$PUBLIC_KEY', '$SHARED_SECRET', '$BASE_URL', '$PRODUCT_TYPE');" >> "$CREATE_FILE"

   for reminderId in `seq 1 $REMINDERS_PER_TENANT`
   do
      SUMMARY=`uuid`
      USER_KEY=`uuid`
      USER_MAIL="$USER_KEY@test.test"
      USER_MESSAGE="User: $USER_KEY Summary: $SUMMARY"
      # This number was chosen so that ~3% of reminders would be expiring and the 
      # index would be used for the lookup. As per: http://stackoverflow.com/a/5203827/83446
      # 32767 * 0.03 = 983 (Three percent the range of random)
      HOURS_INTERVAL=`expr "$RANDOM" '-' '983'` 
      echo "INSERT INTO ping (tenantId, issueId, issueKey, issueSummary, userKey, userEmail, message, date) VALUES ($tenantId, $RANDOM, 'ISSUE-$RANDOM', '$SUMMARY', '$USER_KEY', '$USER_MAIL', '$USER_MESSAGE', current_timestamp + interval '$HOURS_INTERVAL hours');" >> "$CREATE_FILE" 
   done
done

