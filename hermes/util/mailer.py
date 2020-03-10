#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import smtplib, ssl
import logging
from email.message import EmailMessage
from . import helper, cert

log = logging.getLogger(__name__)
EXISTING_CONNECTION = {}

def getSMTPConnection():
  '''
    If there is existing live connection that returns 250, use that connection
    If not, establish a new connection
  '''
  if "defaultMailer" in EXISTING_CONNECTION:
    try:
        status = EXISTING_CONNECTION["defaultMailer"].noop()[0]
    except:  # smtplib.SMTPServerDisconnected
        status = -1
    if status == 250: return EXISTING_CONNECTION["defaultMailer"]
  try:
    smtp = smtplib.SMTP("smtp.vmware.com", "25")
    smtp.ehlo()  # Can be omitted
    EXISTING_CONNECTION["defaultMailer"] = smtp
    return smtp
  except Exception as e:
    log.info(e)
    return None

def send(email, subject, message, senderName=None):
  '''
    Using defaultMailer account supplied in user_config.json,
    Send an email to the recipient with subject and message
  '''
  if ',' in email: email = ', '.join(email.split(','))
  if type(email) is list: email = ', '.join(email) # multiple recipients
  if senderName is None: senderName = "VMware Blockchain (Hermes)"
  try:
    smtp = getSMTPConnection()
    if smtp is None:
      log.info("Failed to get SMTP connection.")
      return False
    msg = EmailMessage()
    msg["Subject"] = subject
    msg["From"] = f'"{senderName}" <svc.blockchain_1@vmware.com>'
    msg["To"] = email
    msg.set_content(message)
    smtp.send_message(msg)
    return True
  except Exception as e:
    log.info(e)
    return False
