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

def tlsCertificate(type):
   path = os.path.dirname(os.path.abspath(__file__))
   scriptPath = os.path.join(".", os.path.dirname(os.path.abspath(__file__)), "generateTlsCerts.sh")
   root_path = os.path.join(".", os.path.dirname(os.path.abspath(__file__)), "root-ca.crt")
   if type == 'server':
      if not os.path.exists(root_path):
         type_root = 'root-ca'
         cmd = [scriptPath, type_root]
         success, stdout = util.helper.execute_ext_command(cmd, timeout=3600, working_dir=path, raise_exception=True)
         log.info("\nSuccess and Output are {} and {}".format(success, stdout))
         assert success, "Error creating root certificates"
   elif type == 'client':
      assert os.path.exists(root_path), "Root certificate must exist"
   else:
      raise Exception("Invalid Input type for tlsCertificate.")

   cmd = [scriptPath, type]
   success, stdout = util.helper.execute_ext_command(cmd, timeout=3600, working_dir=path, raise_exception=True)
   log.info("\nSuccess and Output are {} and {}".format(success, stdout))
   assert success, "Error creating {} certificates".format(type)
   fileList = ["root-ca.crt", type + ".crt", type + '.key']
   certInStrList = []
   for fileName in fileList:
      if "key" in fileName:
         certinBytes = OpenSSL.crypto.load_privatekey(OpenSSL.crypto.FILETYPE_PEM, open(path + '/' + fileName).read())
         certDump = OpenSSL.crypto.dump_privatekey(OpenSSL.crypto.FILETYPE_PEM, certinBytes).decode()
      else:
         certinBytes = OpenSSL.crypto.load_certificate(OpenSSL.crypto.FILETYPE_PEM, open(path + '/' + fileName).read())
         certDump = OpenSSL.crypto.dump_certificate(OpenSSL.crypto.FILETYPE_PEM, certinBytes).decode()
      certInStrList.append(certDump)
   Certificates = namedtuple('Certificates', ['rootCaCrt', type+'Crt', type+'Key'])
   certificate = Certificates(certInStrList[0], certInStrList[1], certInStrList[2])
   return certificate

def getTlsPath():
   return os.path.dirname(os.path.abspath(__file__))
