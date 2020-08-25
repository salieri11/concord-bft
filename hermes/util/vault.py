#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import threading
import os
import json
import time
import requests
import traceback

import util.hermes_logging
log = util.hermes_logging.getMainLogger()

REQ_SESSION = requests.Session()

VAULT_BASE_URL = "http://10.78.20.9:8200/v1/kv/data"
VAULT_PARAM_CACHE = {}
VAULT_IDENTIFIER_PREFIX = "<vault."
VAULT_IMPORT_PREFIX = "+" + VAULT_IDENTIFIER_PREFIX
VAULT_FETCHING_FLAG = "__FETCHING__"
VAULT_PARAM_MAX_RECURSION_DEPTH = 32


def resolve(content, pretty=True):
  '''
    Fully resolves Vault param in string identifier (resursion supported)
    Format: <vault.PARAM_NAME@VERSION...{ .keys | [index] }>
    (e.g. <vault.VMware-Blockchain-SDDC-1-Segments@1.prop1.prop2[3].secret>)
    (Without @ version, latest version is assumed and fetched.)
    
    Recursion supported:
    If <vault.VMware-Blockchain-SDDC-1-Segments.v1> resolved with:
    { "some_key": "<vault.some-other-secret.secret1>" }, the function will go into
    nested fetch until there are no child params left with vault identifier format.

    Nested Imports supported:
    With '+' prefix, parser will import the resolved json object, not the string value.
    { "some_key": "+<vault.my_secret>" } ==> { "some_key": { "secret1": "value" } }
  '''
  try:
    resolved = inflate(content)
    if content.startswith(VAULT_IMPORT_PREFIX) and content.endswith(">"):
      return json.loads(resolved)
    else:
      if pretty and isJsonContent(resolved):
        return json.dumps(json.loads(resolved), indent=4)
      else:
        return resolved
  except Exception as e:
    traceback.print_exc()
    log.error(e)
    return None


def resolveFile(filename, pretty=True):
  '''
    Updates the file by given path, replacing all Vault identifier
    references to Vault-pulled values.
  '''
  with open(filename, "r+") as f:
    content = f.read(); f.seek(0)
    resolved = resolve(content, pretty=pretty)
    if type(resolved) is not str:
      if pretty: resolved = json.dumps(resolved, indent=4)
      else: resolved = json.dumps(resolved)
    f.write(resolved); f.truncate()


def inflate(content, depth=0):
  '''
    Recursively parses through string to resolve all Vault identifiers.
    `content` is string content containing Vault identifiers.
    e.g.  If on Vault kv/my_secret has JSON { "secret": "test_secret" },
    "My secret is <vault.my_secret@1.secret.value>!" ==>  "My secret is test_secret!"
    
    Uses multithreaded parallel fetching while resolving all references.
    All fetched JSON values are cached for o(1) fetch for multiple references
    within the content and all its child references within.
    
    (Max recursion is capped at VAULT_PARAM_MAX_RECURSION_DEPTH)
  '''
  if depth > VAULT_PARAM_MAX_RECURSION_DEPTH:
    raise Exception("Max recursion depth has been reached ({})".format(
                    VAULT_PARAM_MAX_RECURSION_DEPTH))
  content = content.strip()
  isJson = isJsonContent(content)
  if isJson:
    dataObj = recursiveInflateJson(json.loads(content), depth+1)
    return json.dumps(dataObj)
  else:
    if VAULT_IDENTIFIER_PREFIX not in content: return content
    chunks = []; threads = []
    def resolveByName(name, i):
      tryCount = 0; maxTries = 100
      while tryCount < maxTries:
        tryCount += 1
        result = fetchParam(name)
        if result == VAULT_FETCHING_FLAG: time.sleep(0.2)
        else: chunks[i]["outcome"] = result; break
    idenSplit = content.split(VAULT_IDENTIFIER_PREFIX)
    front = idenSplit[0]
    idenSplit = idenSplit[1:]
    chunks = []; threads = []; i = 0
    for substr in idenSplit:
      if not substr: continue
      innerSplit = substr.split(">")
      idenPath = innerSplit[0].split(".")
      rest = innerSplit[1] if len(innerSplit) > 1 else ""
      paramName = idenPath[0]
      if "[" in paramName:
        paramName = paramName.split("[")[0]
        idenPath.insert(1, idenPath[0].replace(paramName, ""))
      chunks.append({ "i": i, "path":idenPath, "outcome": None, "rest": rest })
      thd = threading.Thread(
        target = lambda paramName, i: resolveByName(paramName, i), 
        args = (paramName, i, )
      )
      threads.append(thd); thd.start()
      i += 1
    for thd in threads: thd.join(timeout=30)
    results = []
    for chunk in chunks:
      if len(chunk["path"]) == 1:
        results.append(chunk["outcome"]["serialized"] + chunk["rest"])
      else:
        travelNode = chunk["outcome"]["data"]
        notFound = False
        if chunk["outcome"]["data"]:
          for subkey in chunk["path"][1:]:
            arrayNav = []
            if "[" in subkey and "]" in subkey:
              for subkeyChunk in subkey.split("["):
                if "]" not in subkeyChunk: continue
                arrayNav.append(int(subkeyChunk.split("]")[0]))
            subkey = subkey.split("[")[0]
            if subkey in travelNode: # key travel
              travelNode = travelNode[subkey]
            for arrayIndex in arrayNav: # array travel
              if notFound: continue
              if arrayIndex < 0 or arrayIndex >= len(travelNode):
                notFound = True; break
              travelNode = travelNode[arrayIndex]
            else: notFound = True; break
        else: notFound = True
        if notFound: results.append(chunk["rest"])
        else: results.append(str(travelNode) + chunk["rest"])
    result = front + "".join(results)
    return inflate(result, depth+1)


def recursiveInflateJson(data, depth=0):
  '''
    If data type is `list` or `dict` it will iterate through
    and recursive resolve all Value identifiers. It will import
    the whole JSON object when encountering VAULT_IMPORT_PREFIX,
    otherwise simply string-replaced value.
  '''
  if type(data) is list:
    i = 0
    for item in data:
      if type(item) is list or type(item) is dict:
        data[i] = recursiveInflateJson(item, depth+1)
      elif type(item) is str:
        if item.startswith(VAULT_IMPORT_PREFIX) and item.endswith(">"):
          data[i] = json.loads(inflate(item[1:], depth+1))
        else:
          data[i] = inflate(item, depth+1)
      i += 1
  elif type(data) is dict:
    unsetList = []
    for key in data:
      resolvedKey = inflate(key, depth+1)
      if key != resolvedKey:
        data[resolvedKey] = data[key]
        unsetList.append(key)
    for keyToBeUnset in unsetList:
      del data[keyToBeUnset]
    for key in data:
      item = data[key]
      if type(item) is list or type(item) is dict:
        data[key] = recursiveInflateJson(item, depth+1)
      elif type(item) is str:
        if item.startswith(VAULT_IMPORT_PREFIX) and item.endswith(">"):
          data[key] = json.loads(inflate(item[1:], depth+1))
        else:
          data[key] = inflate(item, depth+1)
  return data


def fetchParam(name, token=None):
  '''
    Takes entry name (e.g. some_secret_name, @ version optional)
    and fetches the json content of the target secret/value
    It will register the fetched value to cache for recurring look-ups.
  '''
  if name in VAULT_PARAM_CACHE: return VAULT_PARAM_CACHE[name]
  VAULT_PARAM_CACHE[name] = VAULT_FETCHING_FLAG
  if not token: token = os.getenv("VAULT_API_TOKEN")
  headers = { "X-Vault-Token": token }
  if "@" in name:
    version = name.split("@")[1]; pureName = name.split("@")[0]
    response = REQ_SESSION.get("{}/{}?version={}".format(VAULT_BASE_URL, pureName, version), headers=headers)
  else:
    response = REQ_SESSION.get("{}/{}".format(VAULT_BASE_URL, name), headers=headers)
  if response.status_code != 200:
    VAULT_PARAM_CACHE[name] = { "data": None, "serialized": "" }
    log.error("Param fetch '{}' has {}'ed".format(name, response.status_code))
    return VAULT_PARAM_CACHE[name]
  content = response.json()["data"]["data"]
  VAULT_PARAM_CACHE[name] = { "data": content, "serialized": json.dumps(content), }
  log.debug("Param fetch '{}' succeeded: {}".format(name, VAULT_PARAM_CACHE[name]["serialized"]))
  return VAULT_PARAM_CACHE[name]


def isJsonContent(content):
  return (content.startswith("{") and content.endswith("}")) or \
         (content.startswith("[") and content.endswith("]"))
