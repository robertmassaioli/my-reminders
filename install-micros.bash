MICROS_VERSION=${MICROS_VERSION:-1.3.0}

wget https://s3-ap-southeast-2.amazonaws.com/atl-software/micros-cli.${MICROS_VERSION}.tar.gz
mkdir -p micros
tar -zxvf micros-cli.${MICROS_VERSION}.tar.gz -C ./micros
rm -f micros-cli.${MICROS_VERSION}.tar.gz
cd micros
npm install --production
chmod +x micros.js
