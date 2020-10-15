#!/bin/bash

mkdir cap
cd cap
echo "Hello User! Welcome to the ova creation wizard!"
echo "This uses CAP to build blockchain appliance. Refer to https://confluence.eng.vmware.com/pages/viewpage.action?spaceKey=CPDVC&title=CAP+-+Getting+Started"
echo "Enter CAP build to use (enter full url to cap zip file)"
read CAP_BUILD
wget $CAP_BUILD
CAP_ZIP=$(ls | grep *.zip)
echo "extracting $CAP_ZIP"
unzip $CAP_ZIP
export PATH=$PWD:$PATH
mv ovftool ovftool4.4.0
ln -s ./ovftool4.4.0/ovftool ovftool
ls -l
./cap --help
./cap init

cp ../appliance.json $PWD
cp ../customizations.json $PWD
cp ../post-install-blockchain.sh $PWD
cp ../vmw_welcome_message.txt $PWD

sed -i "s/PWD/${PWD//\//\\/}/g" appliance.json

echo "Enter version number of the blockchain ovf"
read BCVERSION
sed -i "s/BCVERSION/$BCVERSION/g" appliance.json
sed -i "s/BCVERSION/$BCVERSION/g" customizations.json

echo "ISO_BUILD_URL is the url of the iso to be used."
echo "NOTE: use a build from https://buildweb.eng.vmware.com/ob/?product=securityharden-vmbase-generic&branch=cap&buildtype=release"
echo "Enter ISO_BUILD_URL"
read ISO
sed -i "s#ISO_BUILD_URL#$ISO#g" appliance.json

echo "You should have a VC deployed. Refer to the details of the VC for the below section."
echo "Enter DATASTORE"
read DATASTORE
sed -i "s/DATASTORE/$DATASTORE/g" appliance.json
echo "Enter DATACENTER"
read DATACENTER
sed -i "s/DATACENTER/$DATACENTER/g" appliance.json
echo "Enter VC_ADDRESS"
read VC_ADDRESS
sed -i "s/VC_ADDRESS/$VC_ADDRESS/g" appliance.json
echo "Enter VC_PASSWORD"
read VC_PASSWORD
sed -i "s/VC_PASSWORD/$VC_PASSWORD/g" appliance.json
echo "Enter VC_USERNAME"
read VC_USERNAME
sed -i "s/VC_USERNAME/$VC_USERNAME/g" appliance.json
echo "Enter ESX_HOSTNAME"
read ESX_HOSTNAME
sed -i "s/ESX_HOSTNAME/$ESX_HOSTNAME/g" appliance.json

echo "Refer to the details of the iso used for the below section."
echo "Enter full version of photon OS used"
read FULL_VERSION
sed -i "s/FULL_VERSION/$FULL_VERSION/g" customizations.json

echo "Running CAP appliance creation..."
./cap build -f appliance.json

echo "Complete! Find your ova in the images folder."