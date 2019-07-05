#########################################################################
# Copyright 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import requests
import logging

log = logging.getLogger(__name__)

refresh_token = "a3674390-edb6-4c93-915e-daf741909541"

def getAccessToken():
   stage_url = "https://console-stg.cloud.vmware.com/csp/gateway/am/api/auth/api-tokens/authorize"
   csp_auth_data = requests.post(stage_url, data={"refresh_token": refresh_token}).json()

   log.info('CSP Authorize API Response: {0}'.format(str(csp_auth_data)))

   return csp_auth_data['access_token']
