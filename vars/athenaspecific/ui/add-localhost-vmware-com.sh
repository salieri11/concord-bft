#!/bin/sh
# Copyright 2018-2019 VMware, all rights reserved.


#
# This is needed for the UI to get past CORS issues with CSP
#
echo 0.0.0.0 localhost.vmware.com >> /etc/hosts
