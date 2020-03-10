#!/usr/bin/python3

#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

# Invocation of standalone utility functions that often 
# require Jenkins-injected credentials in user_config

import argparse
import logging
import traceback
from util import helper, slack, mailer

log = None

def slackDM(args, secret):
  a = prepareArgs(args)
  slack.sendMessageToPerson(email=a[0], message=a[1], ts=a[2], token=secret)

def slackPost(args, secret):
  a = prepareArgs(args)
  slack.postMessageOnChannel(channelName=a[0], message=a[1], ts=a[2], token=secret)

def emailSend(args, secret):
  a = prepareArgs(args)
  mailer.send(email=a[0], subject=a[1], message=a[2], senderName=a[3])

# Registry of callable standalone functions
DISPATCH = {
  "emailSend": emailSend,
  "slackDM": slackDM,
  "slackPost": slackPost,
}


def main():
  setUpLogging()
  parser = argparse.ArgumentParser()
  parser.add_argument("funcName", help="Name of the utility function.")
  parser.add_argument("--param",
                      help="arguments to the utility function call",
                      default=[],
                      nargs="*")
  parser.add_argument("--credentials",
                      help="optional credentials parameter to override default value in user_config.json",
                      default="")
  args = parser.parse_args()
  setUpLogging()
  try:
    # remove all leading/trailing quotes in params; groovy calls can inject those in sh block
    for i, param in enumerate(args.param):
      while param.startswith('"') and param.endswith('"'):
        param = param[1:-1]
      args.param[i] = param
    DISPATCH[args.funcName](args.param, args.credentials)
  except Exception as e:
    log.info(e)
    traceback.print_exc()
  return


def setUpLogging():
  try:
    logging.basicConfig(level=logging.DEBUG, format='%(asctime)s %(levelname)s %(message)s',
                        datefmt='%Y-%m-%d %H:%M:%S')
    global log
    log = logging.getLogger(__name__)
  except AttributeError:
    exit(1)


def prepareArgs(args):
  safeArgs = [None]*32
  for i, arg in enumerate(args):
    safeArgs[i] = arg
  return safeArgs


if __name__ == "__main__":
  main()