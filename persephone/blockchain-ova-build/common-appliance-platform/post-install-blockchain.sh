#!/bin/bash
set -x -v

# This is needed to enable docker bridge networking. Ensure that the
# Kernel version on the base image matches this value.
LINUX_KERNEL_VERSION=LINVER

# The base SH photon image does not have docker bridge enabled. Due to this the
# docker service fails to start. Enable it here.
# From:
# https://opengrok.eng.vmware.com/source/xref/main.perforce.1666/bora/install/vpxd/linux/bin/csgw.sh
# Remove "install bridge /bin/false" from modprobe config
echo "Enabling docker bridge networking: kernel version: $LINUX_KERNEL_VERSION"
grep -v 'install bridge' /etc/modprobe.d/modprobe.conf > /tmp/modprobe
mv /tmp/modprobe /etc/modprobe.d/modprobe.conf

if [ -d /usr/lib/modules/$LINUX_KERNEL_VERSION ]
then
    insmod /usr/lib/modules/$LINUX_KERNEL_VERSION/kernel/net/llc/llc.ko.xz
    insmod /usr/lib/modules/$LINUX_KERNEL_VERSION/kernel/net/802/stp.ko.xz
    insmod /usr/lib/modules/$LINUX_KERNEL_VERSION/kernel/net/bridge/bridge.ko.xz

    mkdir /etc/docker
    echo '{"bip": "172.17.0.1/16"}' > /etc/docker/daemon.json

    # Start docker
    systemctl restart docker
    echo "Finished setting up docker bridge"
else
    echo "ERROR: Docker bridge: module directory /usr/lib/modules/$LINUX_KERNEL_VERSION not found"
    echo "ERROR: Docker bridge setup skipped: Docker service will not be available"
    exit -1
fi

echo "Building welcome message from VMWare"
truncate -s 0 /etc/issue
cat /root/blockchain/vmw_welcome_message.txt > /etc/issue