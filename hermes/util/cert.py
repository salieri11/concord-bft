#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import certifi
import ssl
import util.helper
import os
import OpenSSL
from collections import namedtuple

log = util.hermes_logging.getMainLogger()
SCRIPT_DIR = os.path.join(os.path.dirname(os.path.abspath(__file__)), "daml", "request_tool") 
SCRIPT_PATH  = os.path.join(SCRIPT_DIR, "generateTlsCerts.sh")


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
   

def tlsSignedCrt(path, cert_type):
   root_path = os.path.join(path, "root-ca.crt")
   if not os.path.exists(root_path):
      tlsRootCrt(path)
   cmd = [SCRIPT_PATH, cert_type]
   success, stdout = util.helper.execute_ext_command(cmd, timeout=3600, working_dir=path)
   log.debug("\nSuccess and Output are {} and {}".format(success, stdout))
   assert success, "Error creating {} certificates".format(cert_type)
   

def tlsRootCrt(path):
   type_root = 'root-ca'
   cmd = [SCRIPT_PATH, type_root]
   success, stdout = util.helper.execute_ext_command(cmd, timeout=3600, working_dir=path, raise_exception=True)
   log.debug("\nSuccess and Output are {} and {}".format(success, stdout))
   assert success, "Error creating root certificates"


def tlsCreateLoadSignedCrt(cert_type):
   path = SCRIPT_DIR 
   tlsSignedCrt(path=path,cert_type=cert_type)
   
   fileList = ["root-ca.crt", cert_type + ".crt", cert_type + '.key']
   certInStrList = []
   for fileName in fileList:
      if "key" in fileName:
         certinBytes = OpenSSL.crypto.load_privatekey(OpenSSL.crypto.FILETYPE_PEM, open(path + '/' + fileName).read())
         certDump = OpenSSL.crypto.dump_privatekey(OpenSSL.crypto.FILETYPE_PEM, certinBytes).decode()
      else:
         certinBytes = OpenSSL.crypto.load_certificate(OpenSSL.crypto.FILETYPE_PEM, open(path + '/' + fileName).read())
         certDump = OpenSSL.crypto.dump_certificate(OpenSSL.crypto.FILETYPE_PEM, certinBytes).decode()
      certInStrList.append(certDump)
   Certificates = namedtuple('Certificates', ['rootCaCrt', cert_type+'Crt', cert_type+'Key'])
   certificate = Certificates(certInStrList[0], certInStrList[1], certInStrList[2])
   return certificate


def tlsCreateCrt(cert_type):
   path = SCRIPT_DIR 
   tlsSignedCrt(path=path,cert_type=cert_type)
   return path


def tlsInvalidCertificate(cert_type,invalid_path):
   path = os.path.join(SCRIPT_DIR, invalid_path)
   if not os.path.exists(path):
      os.makedirs(path)
   tlsSignedCrt(path=path,cert_type=cert_type)
   return path
