EACJ_USER=${bamboo_micros_user}
EACJ_PASSWORD=${bamboo_micros_password}

npm config set registry=https://npm.atlassian.io

npm login --registry=https://npm-private.atlassian.io --scope=atlassian <<CREDS
$EACJ_USER
$EACJ_PASSWORD
rmassaioli@atlassian.com
CREDS

npm install --production '@atlassian/micros-cli'
