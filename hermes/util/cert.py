#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import certifi
import ssl
from OpenSSL import crypto
import random
import collections

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

def getKey():
   '''
    Generates a random key and returns it
  '''
   key = crypto.PKey()
   key.generate_key(crypto.TYPE_RSA, 4096)
   key = crypto.dump_privatekey(crypto.FILETYPE_PEM, key)
   return key

def createCsr(key, subject):
   '''
    Generates a certificate signing request and saves it on disk
    Returns certificate signing request and key
  '''
   csrReq = crypto.X509Req()
   key = crypto.load_privatekey(crypto.FILETYPE_PEM, key)
   csrReq.get_subject().commonName = subject.commonName
   csrReq.get_subject().countryName = subject.countryName
   csrReq.get_subject().stateOrProvinceName = subject.stateOrProvinceName
   csrReq.get_subject().localityName = subject.localityName
   csrReq.get_subject().organizationName = subject.organizationName
   csrReq.get_subject().organizationalUnitName = subject.organizationalUnitName
   csrReq.get_subject().emailAddress = subject.emailAddress
   csrReq.set_pubkey(key)
   csrReq.sign(key, "sha512")
   return crypto.dump_certificate_request(crypto.FILETYPE_PEM, csrReq)

def getCASignedCert(csr, caCert, caKey):
   '''
    Accepts certificate signing request and signs it with the private key and saves it on disk
    Returns signed certificate and key.
  '''
   caPKey = crypto.load_privatekey(crypto.FILETYPE_PEM, caKey)
   caCert = crypto.load_certificate(crypto.FILETYPE_PEM, caCert)
   pcsr = crypto.load_certificate_request(crypto.FILETYPE_PEM, csr)
   signedCert = crypto.X509()
   pub_key = pcsr.get_pubkey()
   signedCert.set_version(2)
   signedCert.set_serial_number(random.getrandbits(64))
   signedCert.set_pubkey(pub_key)
   signedCert.set_subject(pcsr.get_subject())
   signedCert.set_issuer(caCert.get_subject())
   signedCert.gmtime_adj_notBefore(0)
   signedCert.gmtime_adj_notAfter(31536000)
   signedCert.add_extensions(get_exts(False))
   signedCert.sign(caPKey, "sha512")
   open('server' + ".crt", "wb").write(
      crypto.dump_certificate(crypto.FILETYPE_PEM, signedCert))
   open('server' + ".key", "wb").write(
      crypto.dump_privatekey(crypto.FILETYPE_PEM, pub_key))
   return crypto.dump_certificate(crypto.FILETYPE_PEM, signedCert)

def getSelfSignedCACert(csr, key):
   '''
    Accepts a certificate signing request and self signs it and saves it on the disk
    Also returns the certificate and key
  '''
   cert = crypto.X509()
   key = crypto.load_privatekey(crypto.FILETYPE_PEM, key)
   pcsr = crypto.load_certificate_request(crypto.FILETYPE_PEM, csr)
   cert.set_serial_number(random.getrandbits(64))
   cert.gmtime_adj_notBefore(0)
   cert.gmtime_adj_notAfter(31536000)
   cert.set_issuer(pcsr.get_subject())
   cert.set_pubkey(pcsr.get_pubkey())
   cert.add_extensions(get_exts())
   cert.sign(key, 'sha512')
   open('root-ca' + ".crt", "wb").write(
      crypto.dump_certificate(crypto.FILETYPE_PEM, cert))
   open('root-ca' + ".key", "wb").write(
      crypto.dump_privatekey(crypto.FILETYPE_PEM, key))
   return crypto.dump_certificate(crypto.FILETYPE_PEM, cert)

def get_exts(ca=True):
   ca_sign = 'digitalSignature, keyEncipherment,  Data Encipherment, Certificate Sign'
   ca_no_sign = 'digitalSignature, keyEncipherment,  Data Encipherment'
   if ca:
      exts = [crypto.X509Extension(b"keyUsage", True, bytes(ca_sign, 'utf-8')),
              crypto.X509Extension(b"basicConstraints", True, b"CA:true")]
   else:
      exts = [crypto.X509Extension(b"keyUsage", True, bytes(ca_no_sign, 'utf-8')),
              crypto.X509Extension(b"basicConstraints", True, b"CA:false")]
   return exts

def generateCACertificate(sub):
   ca_key = getKey()
   ca_csr = createCsr(ca_key, sub)
   return (getSelfSignedCACert(ca_csr,ca_key),
              ca_key)

def generateCASignedCert(caKey, caCert, sub):
   priv_key = getKey()
   csr = createCsr(priv_key, sub)
   return (getCASignedCert(csr, caCert, caKey),
           priv_key)

