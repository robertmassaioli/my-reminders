EACJ_USER=${bamboo_micros_user}
EACJ_PASSWORD=${bamboo_micros_password}

if [ "x$EACJ_USER" == "x" ]
then
   echo "Error: no EACJ username was provided"
   exit 1
fi

if [ "x$EACJ_PASSWORD" == "x" ]
then
   echo "Error: no EACJ password was provided"
   exit 1
fi

echo "Install the very latest npm using npm."
npm install npm@2
ln -s ./node_modules/npm/bin/npm .

echo "Setting the npm registry to use the atlassian registry."
./npm config set registry=https://npm.atlassian.io

echo "Logging into the NPM registry with EACJ credentials: $EACJ_USER"
./npm login --registry=https://npm-private.atlassian.io --scope=atlassian <<CREDS
$EACJ_USER
$EACJ_PASSWORD
rmassaioli@atlassian.com
CREDS

echo "Installing micros-cli"
./npm install --production '@atlassian/micros-cli'

echo "micros-cli installed"
