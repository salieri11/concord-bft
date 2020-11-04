#!/usr/bin/env bash

echo "prepare build inputs to CAP..."
source ./build.properties
echo "Copying source to $PWD ..."
cp $BUILD_FILES_REPOSITORY/appliance.json $PWD
cp $BUILD_FILES_REPOSITORY/customizations.json $PWD
cp $BUILD_FILES_REPOSITORY/post-install-blockchain.sh $PWD
cp $BUILD_FILES_REPOSITORY/vmw_welcome_message.txt $PWD

echo "Fetching inputs for blockchain-ova-build..."
sed -i "s/BUILD_FILES_REPOSITORY/${PWD//\//\\/}/g" appliance.json
APPLIANCE_NAME="vmw-blockchain-sh-ovf-$BCVERSION"
sed -i "s/APPLIANCE_NAME/$APPLIANCE_NAME/g" appliance.json
sed -i "s/BCVERSION/$BCVERSION/g" customizations.json
sed -i "s#CAP_BASE_IMAGE#$CAP_BASE_IMAGE#g" appliance.json
sed -i "s/DATASTORE/$DATASTORE/g" appliance.json
sed -i "s/DATACENTER/$DATACENTER/g" appliance.json
sed -i "s/VC_ADDRESS/$VC_ADDRESS/g" appliance.json
sed -i "s/VC_PASSWORD/$VC_PASSWORD/g" appliance.json
sed -i "s/VC_USERNAME/$VC_USERNAME/g" appliance.json
sed -i "s/ESX_HOSTNAME/$ESX_HOSTNAME/g" appliance.json
sed -i "s/FULL_VERSION/$FULL_VERSION/g" customizations.json
sed -i "s/LINVER/$LINVER/g" post-install-blockchain.sh
sed -i "s/NETWORK_SEGMENT/$NETWORK_SEGMENT/g" appliance.json

echo "downloading CAP build..."
wget $CAP_BUILD
CAP_ZIP=$(find . -type f -name "*.zip")
echo "extracting $CAP_ZIP"
unzip $CAP_ZIP
export PATH=$PWD:$PATH
mv ovftool ovftool4.4.0
ln -s ./ovftool4.4.0/ovftool ovftool
ls -l
./cap --help
./cap init

echo "running CAP appliance build..."
CAP_OUTPUT=`./cap build -f appliance.json`

if [ $(echo $CAP_OUTPUT | grep -c "Appliance created successfully" ) -gt 0 ]
then
  echo "CAP Ovf build successfully completed!"
  IMAGE_FOLDER=$(echo $CAP_OUTPUT | sed -n -r -e 's/^.*(images[^[:space:]]*).*$/\1/p')
  CL_NAME="vmw-blockchain-sh-cl-$BCVERSION"
  OVF_CONTENTS="vmw-blockchain-sh-cl-$BCVERSION/vmw-blockchain-sh-ovf-$BCVERSION"
  mkdir $CL_NAME
  mkdir $OVF_CONTENTS
  cp $IMAGE_FOLDER/$APPLIANCE_NAME.ovf $OVF_CONTENTS
  cp $IMAGE_FOLDER/$APPLIANCE_NAME.mf $OVF_CONTENTS
  cp $IMAGE_FOLDER/$APPLIANCE_NAME-disk1.vmdk $OVF_CONTENTS
  cp $IMAGE_FOLDER/$APPLIANCE_NAME-file1.nvram $OVF_CONTENTS

  echo "Creating content library..."
  python3 make_vcsp_2018.py -n S3-Publisher -t local -p $CL_NAME
  echo "Look for errors or find content library in $PWD/$CL_NAME"
  exit 0
else
  echo "CAP Ovf build failed!"
  exit 1
fi
