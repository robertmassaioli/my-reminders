echo "Starting the install micros-cli process."

EACJ_USER=${bamboo_micros_user}
EACJ_PASSWORD=${bamboo_micros_password}
NPM_AUTH_TOKEN=${bamboo_npm_auth_password}

echo "==> Running the pre install checks..."

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

if [ "x$NPM_AUTH_TOKEN" == "x" ]
then
   echo "Error: no NPM auth token was provided"
   exit 1
fi

echo "Install the very latest npm using npm."
npm install npm@2
ln -s ./node_modules/npm/bin/npm .

echo "Setting the npm registry to use the atlassian registry."
./npm config set registry=https://npm.atlassian.io

echo "Logging into the NPM registry with EACJ credentials: $EACJ_USER"
##./npm login --registry=https://npm-private.atlassian.io --scope=atlassian <<CREDS
##$EACJ_USER
##$EACJ_PASSWORD
##rmassaioli@atlassian.com
##CREDS
cat > ~/.npmrc <<RAWDOC
registry=https://npm.atlassian.io/
@atlassian:registry=https://npm-private.atlassian.io/
//npm-private.atlassian.io/:_authToken=$NPM_AUTH_TOKEN
RAWDOC

if ! grep '//npm-private.atlassian.io/:_authToken' ~/.npmrc > /dev/null
then
   echo "Error: You have failed to generate an authentication token for NPM."
   exit 2
fi

echo "Installing micros-cli"
./npm install --production '@atlassian/micros-cli'

echo "micros-cli installed"
