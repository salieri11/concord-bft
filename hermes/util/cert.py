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

def tlsCertificate():
   path = os.path.dirname(os.path.abspath(__file__))
   scriptPath = os.path.join(".",os.path.dirname(os.path.abspath(__file__)), "generateTlsCerts.sh")
   cmd = [scriptPath]
   success, stdout = util.helper.execute_ext_command(cmd, timeout=3600, working_dir=path,raise_exception=True)
   log.info("\nSuccess and Output are {} and {}".format(success, stdout))
   assert success, "Error creating Certificates"
   fileList = ["root-ca.crt", "server.crt", 'server.key']
   certInStrList = []
   for fileName in fileList:
      if fileName[-3:] == "key":
         certinBytes = OpenSSL.crypto.load_privatekey(OpenSSL.crypto.FILETYPE_PEM, open(path+'/'+fileName).read())
         certDump = OpenSSL.crypto.dump_privatekey(OpenSSL.crypto.FILETYPE_PEM, certinBytes).decode()
      else:
         certinBytes = OpenSSL.crypto.load_certificate(OpenSSL.crypto.FILETYPE_PEM, open(path+'/'+fileName).read())
         certDump = OpenSSL.crypto.dump_certificate(OpenSSL.crypto.FILETYPE_PEM, certinBytes).decode()
      certInStrList.append(certDump)
   Certificates = namedtuple('Certificates', ['rootCaCrt', 'serverCrt', 'serverKey'])
   certificate = Certificates(certInStrList[0], certInStrList[1], certInStrList[2])
   return certificate
