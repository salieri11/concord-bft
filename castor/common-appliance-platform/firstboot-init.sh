#!/bin/bash

# Firstboot shell script. Sets up guest customizations for each appliance OVF
# template installation.
# Mostly copied from: srm.perforce-aog.1750:/srm/main/install/cap/scripts/firstboot-init.sh

echo "Firstboot: setting root password"
rootpwd=$(/opt/vmware/bin/ovfenv -q --key root-password)
if [ "$?" -eq 0 ]
then
    if [ -n $rootpwd ]
    then
        (echo "$rootpwd" ; echo "$rootpwd") | passwd || \
            echo "Firstboot: error setting the root password. Password requires at least 8 symbols and 4 character types. Please use the default password to connect."
        # Clear root-password property
        /opt/vmware/bin/ovfenv -q --key root-password --value '' || true
    fi
fi

echo "Firstboot: setting blockchain password"
blockchainpwd=$(/opt/vmware/bin/ovfenv -q --key blockchain-password)
if [ "$?" -eq 0 ]
then
    if [ -n $blockchainpwd ]
    then
        (echo "$blockchainpwd" ; echo "$blockchainpwd") | passwd blockchain || \
            echo "Firstboot: error setting the blockchain password. Password requires at least 8 symbols and 4 character types. blockchain user password not set"
        # Clear blockchain-password property
        /opt/vmware/bin/ovfenv -q --key blockchain-password --value '' || true
    fi
fi

#regenerate host keys
echo "Firstboot: regenerating host keys"
systemctl stop sshd
rm -f /etc/ssh/ssh_host*key*
/usr/bin/ssh-keygen -A
systemctl start sshd
