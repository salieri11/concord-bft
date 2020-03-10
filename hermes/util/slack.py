#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import requests
import json
import logging
import json
import slack
from . import helper, cert

log = logging.getLogger(__name__)
reqSession = requests.Session()

def sendMessageToPerson(email, message, ts=None, token=None):
  '''
    Using VMware internal Slack space, send a direct message (DM)
    to a person by looking up their Slack ID with supplied email,
    then, using Slack ID, send a DM with supplied message.
    `ts` denotes thread (if supplied, bot will send message as a reply to a thread)
  '''
  slackConfig = helper.loadConfigFile()["communication"]["slack"]
  token = token if token else slackConfig["workspaces"]["vmware"]["appToken"]
  client = slack.WebClient(token=token, ssl=cert.getSecureContext())
  
  userData = client.users_lookupByEmail(
    email = email
  )
  if userData["ok"] is not True:
    log.info(f"Cannot look up user_id given email {email}, response not OK.\nResponse Body: {json.dumps(userData)}")
    return
  
  userId = userData["user"]["id"]
  log.info(f"Sending Slack Direct Message to '{email}' ({userId})...")
  responseData = client.chat_postMessage(
    channel = userId,
    text = message,
    thread_ts = ts
  )
  if responseData["ok"] is not True:
    log.info(f"Cannot send message to {userId}, response not OK.\nResponse Body: {json.dumps(responseData)}")
    return
  return responseData
  

def postMessageOnChannel(channelName, message, ts=None, token=None):
  '''
    Look up the workspace the channel resides in (VMware Internal? VMW+DA?)
    And using workspace api token, post a message on a known registered Slack channel.
    `ts` denotes thread (if supplied, bot will send message as a reply to a thread)
  '''
  slackConfig = helper.loadConfigFile()["communication"]["slack"]
  if channelName not in slackConfig["channels"]:
    log.info(f"Channel name '{channelName}' is not registered on user_config.json")
    return
  channelId = slackConfig["channels"][channelName]["channelId"]
  channelWorkspace = slackConfig["channels"][channelName]["workspace"]
  token = token if token else slackConfig["workspaces"][channelWorkspace]["appToken"]
  client = slack.WebClient(token=token, ssl=cert.getSecureContext())

  log.info(f"Posting slack message on channel '{channelName}'...")
  responseData = client.chat_postMessage(
    channel = channelId,
    text = message,
    thread_ts = ts
  )
  if responseData["ok"] is not True:
    log.info(f"Cannot post message on '{channelName}', response not OK.\nResponse Body: {json.dumps(responseData)}")
    return
  return responseData
