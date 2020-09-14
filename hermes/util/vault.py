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

REQ_SESSION = requests.Session() # in case of TLS, session will avoid multiple handshakes

VAULT_BASE_URL = "http://10.78.20.9:8200/v1/kv/data" # main Vault endpoint for kv store.
VAULT_PARAM_CACHE = {} # cached Vault kv param look-up responses to avoid dupe look-ups
VAULT_IDENTIFIER_PREFIX = "<vault." # default identifier to look up
VAULT_IMPORT_PREFIX = "+" + VAULT_IDENTIFIER_PREFIX # "+<vault." will import the raw JSON into object key
VAULT_FETCHING_FLAG = "__FETCHING__" # fetching flag for letting threads no it's being fetched.
VAULT_PARAM_MAX_RECURSION_DEPTH = 32 # prevent unending recursiong where there is circular look-up of Vault params


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

    Keys containing dots (with single quotes):
    { "some_key": "<vault.my_secret.'keys.with.dot'.'dot.key2'>" }
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

  if depth > VAULT_PARAM_MAX_RECURSION_DEPTH: # prevent circular ref; stack overflow
    raise Exception("Max recursion depth has been reached ({})".format(
                    VAULT_PARAM_MAX_RECURSION_DEPTH))
  
  if isJsonContent(content):
    # If file content is JSON, first decode it, and
    # recursively navigate into the structure to find `<vault.` identifiers
    # in the JSON content
    dataObj = recursiveInflateJson(json.loads(content), depth+1)
    return json.dumps(dataObj) # file content is string to dump that JSON back to string

  else:
    
    # no need to resolve if there are no `<vault.` identifiers
    if VAULT_IDENTIFIER_PREFIX not in content: return content
    
    # Split all identifiers to structure (front + [iden1+rest1, iden2+rest2, ... idenN+restN]:
    # e.g. "This is front part <vault.My-Secret-1.secret> and middle content and <vault.My-Secret-2.secret> and rest."
    # front = "This is front part "
    # chunk1 = "<vault.My-Secret-1.secret> and middle content and "
    # chunk2 = "<vault.My-Secret-2.secret> and rest."
    idenSplit = content.split(VAULT_IDENTIFIER_PREFIX)
    front = idenSplit[0]; idenSplit = idenSplit[1:]
    i = 0 # chunk counter
    
    # fetch ALL identifiers in parallel, caching them as proceeding
    chunks = []; threads = []
    def resolveByName(name, i): # function used for concurrent look up
      tryCount = 0; maxTries = 150 # with 0.1 wait per tick; max timeout of 15s
      while tryCount < maxTries:
        tryCount += 1
        result = fetchParam(name) # this uses caching; same iden is fetched only once.
        if result == VAULT_FETCHING_FLAG:
          # some other thread is already fetching that same identifer from Vault;
          # so this thread can wait for that thread to fetch the exact same iden
          time.sleep(0.1)
        else:
          chunks[i]["outcome"] = result
          break
    
    # process all chunks and resolve Vault entries in parallel
    for substr in idenSplit:
      if not substr: continue
      innerSplit = substr.split(">")
      idenPath = innerSplit[0].split(".")
      rest = innerSplit[1] if len(innerSplit) > 1 else ""
      paramName = idenPath[0] # <vault.My-Secret.propname> ==> "My-Secret"
      if "[" in paramName: # exclude index part e.g. [1] from <vault.My-Secret[1]>
        paramName = paramName.split("[")[0]
        idenPath.insert(1, idenPath[0].replace(paramName, "")) # only preserve
      if "'" in innerSplit[0]: # has dot-containing key (e.g. My-Secret.'key.with.dot')
        quotesSplit = '.'.join(idenPath[1:]).split("'")
        q = 0; idenPath = [paramName]
        for quotedKey in quotesSplit:
          quoteOpen = (q % 2 == 1) # odd means quotes are open
          if quoteOpen: idenPath.append(quotedKey)
          else:
            for unquotedKey in quotedKey.split('.'):
              if unquotedKey: idenPath.append(unquotedKey)
          q += 1
      # all chunks with these properties for future processing
      chunks.append({
        "i": i, # chunk index
        "path": idenPath, # e.g. <vault.My-Secret-1.secret> ==> "My-Secret-1" (the Vault key in kv)
        "outcome": None, # what did Vault with? e.g. <vault.My-Secret-1.secret> ==> "my_secret_1"
        "rest": rest # rest part of the chunk
      })
      thd = threading.Thread(
        target = lambda paramName, i: resolveByName(paramName, i), 
        args = (paramName, i, )
      )
      threads.append(thd); thd.start()
      i += 1
    for thd in threads: thd.join(timeout=20)

    # All requrests returned; proceed to replace `<vault.` idens with actual value
    results = [] # store all replaced value of chunks in this
    for chunk in chunks:
      if len(chunk["path"]) == 1: # just <vault.My-Secret> (whole json)
        results.append(chunk["outcome"]["serialized"] + chunk["rest"])
      else: # has subpaths
        travelNode = chunk["outcome"]["data"]
        notFound = False
        if chunk["outcome"]["data"]: # data contains whole <vault.My-Secret.*>
          # Now we have the whole JSON object, make identifier travel by given path
          # e.g.  <vault.My-Secret.secret.subtype.array[3].subtype[8].my_secret>
          for subkey in chunk["path"][1:]: # first is `My-Secret`; start from inner prop look-up
            # Initial object property look up doesn't need index so save the array indexes;
            # e.g. <vault.My-Secret.secrets[0]> ==> 'secrets' prop look-up first and then [0]
            arrayNav = []
            if "[" in subkey and "]" in subkey:
              for subkeyChunk in subkey.split("["):
                if "]" not in subkeyChunk: continue
                arrayNav.append(int(subkeyChunk.split("]")[0]))
            # prop look-up always comes first if propname is exists
            subkey = subkey.split("[")[0]
            if subkey in travelNode:
              travelNode = travelNode[subkey]
            # Then, array index travel if specified e.g. <vault.My-Secret.secrets[0][1]>
            if arrayNav: 
              for arrayIndex in arrayNav:
                if notFound: continue
                if type(travelNode) is not list:
                  notFound = True; break
                if arrayIndex < 0 or arrayIndex >= len(travelNode):
                  notFound = True; break
                travelNode = travelNode[arrayIndex]
              else: notFound = True; break
        else: notFound = True # if there is no <vault.My-Secret> on Vault, then not found.
        if notFound:
          # if not found, just consider the whol identifier <vault.My-Secret> as empty string
          # and concat with keeping the rest of the content that are not Vault identifiers.
          # e.g. "my secret is <vault.My-Secret-404.stuff> !!" ==> "my secret is  !!"
          log.info("Vault identifier '{}' not found.".format(".".join(chunk["path"])))
          results.append(chunk["rest"])
        else:
          # If index look-up returned with some array/object, then json encode it
          if type(travelNode) is list or type(travelNode) is dict:
            results.append(json.dumps(travelNode) + chunk["rest"])
          else: # if not default is str value since target file contents are always string
            results.append(str(travelNode) + chunk["rest"])
    # Concat all resolved look-ups 
    # e.g. "This is front part <vault.My-Secret-1.secret> and middle content and <vault.My-Secret-2.secret> and rest."
    #  ==> "This is front part " + ("SECRET1 and middle content and") + ("SECRET2 and rest.")
    #  ==> "This is front part SECRET1 and middle content and SECRET2 and rest."
    result = front + "".join(results)
    # Resursively resolve until there is no more `<vault.` identifiers returned by the responses.
    # e.g. `<vault.My-Secret-1.secret>` might have returned with "<vault.My-Inner-Secret> and <vault.My-Inner-Secret2>"
    return inflate(result, depth+1)


def recursiveInflateJson(data, depth=0):
  '''
    If data type is `list` or `dict` it will iterate through
    and recursive resolve all Value identifiers. It will import
    the whole JSON object when encountering VAULT_IMPORT_PREFIX,
    otherwise simply string-replaced value.
  '''
  # navigate array to find vault idens
  if type(data) is list:
    i = 0
    for item in data:
      if type(item) is list or type(item) is dict:
        data[i] = recursiveInflateJson(item, depth+1)
      elif type(item) is str:
        if item.startswith(VAULT_IMPORT_PREFIX) and item.endswith(">"):
          # if "+<vault.", import the whole raw object into the key
          data[i] = json.loads(inflate(item[1:], depth+1))
        else: # otherwise treat as string content value
          data[i] = inflate(item, depth+1)
      i += 1
  
  # navigate object to find vault idens
  elif type(data) is dict:
    # if keys themselves are Vault idens "<vault.*"
    # replace those keys with resolved values
    # e.g. { "<vault.My-Secret.secret>": "something" } ==> { "SECRET1": "something" }
    unsetList = []
    for key in data:
      resolvedKey = inflate(key, depth+1)
      if key != resolvedKey:
        data[resolvedKey] = data[key]
        unsetList.append(key)
    for keyToBeUnset in unsetList: # unset vault iden keys
      del data[keyToBeUnset]
    # After all vault iden keys are resolved, navigate through object
    # to resolve all Vault identifiers in the content of the property
    for key in data:
      item = data[key]
      if type(item) is list or type(item) is dict:
        data[key] = recursiveInflateJson(item, depth+1)
      elif type(item) is str:
        if item.startswith(VAULT_IMPORT_PREFIX) and item.endswith(">"):
          # if "+<vault.", import the whole raw object into the key
          data[key] = json.loads(inflate(item[1:], depth+1))
        else: # otherwise treat as string content value
          data[key] = inflate(item, depth+1)
  # return object/array whose content has been recursively naviagated
  # for replacing all vault identifiers into actual Vault values.
  return data


def fetchParam(name, token=None):
  '''
    Takes entry name (e.g. some_secret_name, @ version optional)
    and fetches the json content of the target secret/value
    It will register the fetched value to cache for recurring look-ups.
  '''
  # if there is cached value, no need to look-up; just return that.
  if name in VAULT_PARAM_CACHE:
    return VAULT_PARAM_CACHE[name]
  else:
    # before full HTTP look-up, mark this thread as reserved for fetching that Vault param
    # so that no other thread tries to look-up this param that is yet to be resolved.
    VAULT_PARAM_CACHE[name] = VAULT_FETCHING_FLAG
  
  # use vault api token from environment variable if not supplied
  if not token: token = os.getenv("VAULT_API_TOKEN")
  headers = { "X-Vault-Token": token }
  
  # Only honor version if @ version symbol is set
  if "@" in name:
    version = name.split("@")[1]; pureName = name.split("@")[0]
    response = REQ_SESSION.get("{}/{}?version={}".format(VAULT_BASE_URL, pureName, version), headers=headers)
  else:
    response = REQ_SESSION.get("{}/{}".format(VAULT_BASE_URL, name), headers=headers)
  # if not 200, consider resolved value as empty string and cache it
  if response.status_code != 200:
    VAULT_PARAM_CACHE[name] = { "data": None, "serialized": "" }
    log.error("Param fetch '{}' has {}'ed".format(name, response.status_code))
    return VAULT_PARAM_CACHE[name]
  # if found well, resolve content (Vault uses JSON) as serialized JSON
  # and cache it so that unneccessary dupe look-ups are avoided.
  content = response.json()["data"]["data"]
  VAULT_PARAM_CACHE[name] = { "data": content, "serialized": json.dumps(content), }
  log.debug("Param fetch '{}' succeeded: {}".format(name, VAULT_PARAM_CACHE[name]["serialized"]))
  return VAULT_PARAM_CACHE[name]


def isJsonContent(content):
  content = content.strip()
  return (content.startswith("{") and content.endswith("}")) or \
         (content.startswith("[") and content.endswith("]"))
