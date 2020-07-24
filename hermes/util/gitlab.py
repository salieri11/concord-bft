#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import logging
import traceback
import time
import requests
import os
import json
import urllib.parse
from . import helper, hermes_logging
log = hermes_logging.getMainLogger()

DEFAULT_PROJECT_ID = "19894" # blockchain/vmwathena_blockchain repo
BASE_URL = "https://gitlab.eng.vmware.com"
BASE_API_URL = BASE_URL + '/api/v4'
BASE_PROJECT_URL = BASE_API_URL + '/projects/' + DEFAULT_PROJECT_ID
REQ_SESSION = requests.Session() # keep connection alive to avoid excessive HTTPS handshakes if possible


def getMRDetails(mrNumber):
  '''Get metadata on givne MR number (e.g. !2943)'''
  mrNumber = str(mrNumber)
  res = requestWithPathAndParams("GET", BASE_PROJECT_URL, '/merge_requests/' + mrNumber)
  if not res: return None
  return json.loads(res)


def requestWithPathAndParams(method, baseUrl, requestPath, params={}):
  '''
    Since {reqPath + param} format is recurring in all request methods
    Make a helper function to reduce code size & complexity
  '''
  try:
    apiToken = helper.getUserConfig()["gitlab"]["token"]
    paramsValidated = {}
    for paramName in params: # ignore all dict entry with None value
      if params[paramName] is not None:
        paramsValidated[paramName] = params[paramName]
    urlEncodedParams = urllib.parse.urlencode(paramsValidated)
    headers = { "Private-Token": apiToken }
    if method == "GET":
      response = REQ_SESSION.get( url = baseUrl + requestPath + "?" + urlEncodedParams, headers=headers)
    elif method == "POST":
      response = REQ_SESSION.get( url = baseUrl + requestPath, params=params, headers=headers)
    else:
      return None
    if response.status_code == 200:
      return str(response.content.decode('utf-8'))
    else:
      content = response.content.decode('utf-8')
      log.info("Gitlab API {} has returned with {}\ncontent: {}"
                .format(baseUrl + requestPath, response.status_code, content))
      return None
  except Exception as e:
    helper.hermesNonCriticalTrace(e)
    return None

