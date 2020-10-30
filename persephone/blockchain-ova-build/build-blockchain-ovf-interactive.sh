#!/bin/bash

echo "Hello User! Welcome to the ova creation wizard!"
echo "This uses CAP to build blockchain appliance. Refer to https://confluence.eng.vmware.com/pages/viewpage.action?spaceKey=CPDVC&title=CAP+-+Getting+Started"
echo "Enter input repository (containing appliance.json)"
read BUILD_FILES_REPOSITORY
echo "Copying source to $PWD ..."
cp $BUILD_FILES_REPOSITORY/appliance.json $PWD
cp $BUILD_FILES_REPOSITORY/customizations.json $PWD
cp $BUILD_FILES_REPOSITORY/post-install-blockchain.sh $PWD
cp $BUILD_FILES_REPOSITORY/vmw_welcome_message.txt $PWD

echo "Enter CAP build to use (enter full url to cap zip file)"
read CAP_BUILD
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

echo "Fetching inputs for blockchain-ova-build..."
sed -i "s/BUILD_FILES_REPOSITORY/${PWD//\//\\/}/g" appliance.json

echo "Enter version number of the blockchain ovf"
read BCVERSION
APPLIANCE_NAME="vmw-blockchain-sh-ovf-$BCVERSION"
sed -i "s/APPLIANCE_NAME/$APPLIANCE_NAME/g" appliance.json
sed -i "s/BCVERSION/$BCVERSION/g" customizations.json

echo "CAP_BASE_IMAGE is the url of the iso to be used."
echo "NOTE: use a build from https://buildweb.eng.vmware.com/ob/?product=securityharden-vmbase-generic&branch=cap&buildtype=release"
echo "Enter CAP_BASE_IMAGE"
read ISO
sed -i "s#CAP_BASE_IMAGE#$ISO#g" appliance.json

echo "You should have a VC deployed. Refer to the details of the VC for the below section."
echo "Enter VC_ADDRESS"
read VC_ADDRESS
sed -i "s/VC_ADDRESS/$VC_ADDRESS/g" appliance.json
echo "Enter VC_USERNAME"
read VC_USERNAME
sed -i "s/VC_USERNAME/$VC_USERNAME/g" appliance.json
echo "Enter VC_PASSWORD"
read VC_PASSWORD
sed -i "s/VC_PASSWORD/$VC_PASSWORD/g" appliance.json
echo "Enter DATASTORE"
read DATASTORE
sed -i "s/DATASTORE/$DATASTORE/g" appliance.json
echo "Enter DATACENTER"
read DATACENTER
sed -i "s/DATACENTER/$DATACENTER/g" appliance.json
echo "Enter ESX_HOSTNAME"
read ESX_HOSTNAME
sed -i "s/ESX_HOSTNAME/$ESX_HOSTNAME/g" appliance.json
echo "Enter NETWORK_SEGMENT"
read NETWORK_SEGMENT
sed -i "s/NETWORK_SEGMENT/$NETWORK_SEGMENT/g" appliance.json

echo "Refer to the details of the iso used for the below section."
echo "Enter full version of photon OS used"
read FULL_VERSION
sed -i "s/FULL_VERSION/$FULL_VERSION/g" customizations.json

echo "Find your linux kernel version by executing 'cat /etc/lsb-release' or 'uname -a' if you have a VM from the iso you want to deploy"
echo "If otherwise, get the information from the sh-iso team or enter 4.19.129-1.ph3 for photon-3.0."
echo "Enter linux kernel version"
read LINVER
sed -i "s/LINVER/$LINVER/g" post-install-blockchain.sh

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

