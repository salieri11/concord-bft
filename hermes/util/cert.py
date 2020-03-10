#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import certifi
import ssl

def getSecureContext():
  ctx = ssl.create_default_context(cafile=certifi.where())
  ctx.check_hostname = True
  ctx.verify_mode = ssl.CERT_REQUIRED
  return ctx

def getInsecureContext():
  ctx = ssl.create_default_context()
  ctx.check_hostname = False
  ctx.verify_mode = ssl.CERT_NONE
  return ctx