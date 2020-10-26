#!/usr/bin/env bash
# Firstboot shell script. Sets up guest customizations for each appliance OVF template installation.
# Mostly copied from: srm.perforce-aog.1750:/srm/main/install/cap/scripts/firstboot-init.sh

# Set up passwords
echo "Firstboot: setting root password"
rootpwd=$(/opt/vmware/bin/ovfenv -q --key root-password)
if [ "$?" -eq 0 ]
then
    if [ -n "$rootpwd" ]
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
    if [ -n "$blockchainpwd" ]
    then
        (echo "$blockchainpwd" ; echo "$blockchainpwd") | passwd blockchain || \
            echo "Firstboot: error setting the blockchain password. Password requires at least 8 symbols and 4 character types. blockchain user password not set"
        # Clear blockchain-password property
        /opt/vmware/bin/ovfenv -q --key blockchain-password --value '' || true
    fi
fi

# Set up networks
echo "Firstboot: setting network properties"
hostname=$(/opt/vmware/bin/ovfenv -q --key hostname)
hostnamerc=$?
ipaddress=$(/opt/vmware/bin/ovfenv -q --key netipaddress)
ipaddressrc=$?
prefix=$(/opt/vmware/bin/ovfenv -q --key netprefix)
prefixrc=$?
gateway=$(/opt/vmware/bin/ovfenv -q --key netgateway)
gatewayrc=$?
dns=$(/opt/vmware/bin/ovfenv -q --key netdns)
dnsrc=$?

if [[ "$hostnamerc" == 0 ]]
then
  echo "Firstboot: user-provided hostname: $hostname"
  if [[ -n "$hostname" ]]
  then
    # Photon automatically sets the hostname via kickstart to the "hostname" property,
    # so we don't need to explicitly set it here. But make sure it was set.
    # The output goes to journal.
    hostnamectl status
  fi
fi

configureDhcp=false
# If any of the networking properties are not provided, skip setting static IP
if [[ "$ipaddressrc" == 0 ]] && [[ "$prefixrc" == 0 ]] && [[ "$gatewayrc" == 0 ]] && [[ "$dnsrc" == 0 ]]
then
  echo "Firstboot: user-provided ip address: $ipaddress"
  if [[ -n "$ipaddress" ]]
  then
    # Assume all other properties are provided
    echo "Firstboot: setting ip address to: $ipaddress"
    echo "Firstboot: setting prefix to: $prefix"
    echo "Firstboot: setting gateway to: $gateway"
    echo "Firstboot: setting dns to: $dns"

    # Create a static IP configuration
    cat > /etc/systemd/network/10-static-en.network <<EOF
[Match]
Name=eth0

[Network]
Address=$ipaddress/$prefix
Gateway=$gateway
DNS=$dns
EOF

    # restart the network service for the settings to take effect
    systemctl daemon-reload
    echo "FirstBoot: Restarting systemd-networkd"
    systemctl restart systemd-networkd
    echo "FirstBoot: Restarting systemd-resolved"
    systemctl restart systemd-resolved
  else
    echo "Firstboot: Configuring network for DHCP - ip address not set"
    configureDhcp=true
  fi
else
  # User either did not provide any static IP configs, or only provided some. In either case, stick with
  # the default DHCP configuration
  echo "Firstboot: Network property retcodes: ipaddress: $ipaddressrc, prefix: $prefixrc, gateway: $gatewayrc, dns: $dnsrc"
  echo "Configuring network for DHCP"
  configureDhcp=true
fi

if [[ "$configureDhcp" == "true" ]]
then
  echo "Firstboot: Setting up DHCP"
  cat > /etc/systemd/network/10-dhcp-en.network <<EOF
[Match]
Name=eth0

[Network]
DHCP=yes
IPv6AcceptRA=no
EOF
  # restart the network service for the settings to take effect
  systemctl daemon-reload
  echo "FirstBoot: Restarting systemd-networkd"
  systemctl restart systemd-networkd
  echo "FirstBoot: Restarting systemd-resolved"
  systemctl restart systemd-resolved
fi

# Set up EULA acceptance scripts for root and blockchain users
# Add the eula script to bashrc for root.
EULA_DIR=/opt/vmware/blockchain/eula
echo >> /root/.bash_profile
echo "#EULA agreement" >> /root/.bash_profile
echo "source $EULA_DIR/eula.script" >> /root/.bash_profile
echo >> /root/.bash_profile
chmod 644 /root/.bash_profile

# Add the eula script to bashrc for blockchain.
echo >> /home/blockchain/.bashrc
echo "#EULA agreement" >> /home/blockchain/.bashrc
echo "source $EULA_DIR/eula.script" >> /home/blockchain/.bashrc
echo >> /home/blockchain/.bashrc

#regenerate host keys
echo "Firstboot: regenerating host keys"
systemctl stop sshd
rm -f /etc/ssh/ssh_host*key*
/usr/bin/ssh-keygen -A
systemctl start sshd


