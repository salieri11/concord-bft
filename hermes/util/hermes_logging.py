#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import os
import logging
import sys
import traceback

def getMainLogger():
    '''
    Returns the main logger used throughout Hermes.
    '''
    return logging.getLogger("main")


def getHermesLoggingFormatter():
    '''
    Ensure that all Hermes file and console logging has the same format.
    '''
    return logging.Formatter('%(asctime)s %(levelname)s %(message)s',
                             datefmt='%Y-%m-%d %H:%M:%S')


def addStreamHandler(level):
   '''
   Add a stdout handler.  (Python defaults to stderr.)
   '''
   streamHandler = logging.StreamHandler(sys.stdout)
   streamHandler.setLevel(level)
   formatter = getHermesLoggingFormatter()
   streamHandler.setFormatter(formatter)
   getMainLogger().addHandler(streamHandler)


def addFileHandler(fileName, level):
   '''
   Creates a FileHandler.
   Returns the handler, which is needed if one wants to remove it later.
   e.g. A test suite can create and add a handler when it begins, and remove
   that handler when it ends.
   '''
   logDir = os.path.dirname(fileName)

   if not os.path.exists(logDir):
       os.mkdir(logDir)

   fileHandler = None
   try: # if permissions fail, ignore (most likely from invoke.py without sudo)
      fileHandler = logging.FileHandler(fileName, 'w')
      fileHandler.setLevel(level)
      formatter = getHermesLoggingFormatter()
      fileHandler.setFormatter(formatter)
      getMainLogger().addHandler(fileHandler)
   except: pass
   return fileHandler


def logException(exType, exValue, exTb):
   '''
   Python provides hooks (e.g. sys.excepthook) to catch exceptions in logs.
   We set up these hooks to use this function.  If we don't, then exceptions
   will only be written to console, and a test suite's log won't show them.
   '''
   lines = traceback.format_exception(exType, exValue, exTb)
   traceString = ""
   for line in lines:
      traceString += line

   getMainLogger().error(traceString)


def logStringToInt(s):
   '''
   Convert the given log level string, like "debug", to an int.
   '''
   stringLevel = s.upper()
   return getattr(logging, stringLevel)


def setUpLogging(args):
   '''
   Given the args passed to Hermes, sets up Hermes logging.
   '''
   getMainLogger().setLevel(args.logLevel)

   # Console output for the entire run.
   addStreamHandler(args.logLevel)

   # File output for the entire run.
   fullFile = os.path.join(args.resultsDir, "full.log")
   addFileHandler(fullFile, args.logLevel)

   sys.excepthook = logException
   sys.unraisablehook = logException
