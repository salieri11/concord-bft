import collections
import json
import logging

import util.hermes_logging
log = util.hermes_logging.getMainLogger()

def readJsonFile(path):
   '''
   Reads a json file and returns the loaded data.
   On error, displays a message and returns None.
   Some ethereum tests contain tab characters in their json, which is
   invalid. Set strict to False to accept that data.
   '''
   pythonDict = None
   stringData = None

   with open(path) as f:
      stringData = f.read()

   decoder = json.JSONDecoder(strict=False,
                              object_pairs_hook=collections.OrderedDict)

   try:
      pythonDict = decoder.decode(stringData)
   except IOError:
      log.error("The file '{}' could not be read; it may be " \
                "missing or have restricted permissions.".format(path))
   except json.JSONDecodeError as e:
      log.error("The file '{}' could not be parsed as json. " \
                "Error: '{}'".format(path, e))

   return pythonDict
