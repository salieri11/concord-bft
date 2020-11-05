#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import certifi
import ssl
from OpenSSL import crypto
import random

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
   return key

def getSelfSignedCACert(csr, key, name):
   '''
    Accepts a certificate signing request and self signs it and saves it on the disk
    Also returns the certificate and key
  '''
   cert = crypto.X509()
   pcsr = crypto.load_certificate_request(crypto.FILETYPE_PEM, csr)
   cert.set_serial_number(random.getrandbits(64))
   cert.gmtime_adj_notBefore(0)
   cert.gmtime_adj_notAfter(31536000)
   cert.set_issuer(cert.get_subject())
   cert.set_pubkey(pcsr.get_pubkey())
   cert.sign(key, 'sha512')
   open(name + ".crt", "wb").write(
      crypto.dump_certificate(crypto.FILETYPE_PEM, cert))
   open(name + ".key", "wb").write(
      crypto.dump_privatekey(crypto.FILETYPE_PEM, key))
   return (crypto.dump_certificate(crypto.FILETYPE_PEM, cert),
           crypto.dump_privatekey(crypto.FILETYPE_PEM, key))

def createCsr(key, subject, name):
   '''
    Generates a certificate signing request and saves it on disk
    Returns certificate signing request and key
  '''
   csrReq = crypto.X509Req()
   csrReq.get_subject().commonName = subject.commonName
   csrReq.get_subject().countryName = subject.countryName
   csrReq.get_subject().stateOrProvinceName = subject.stateOrProvinceName
   csrReq.get_subject().localityName = subject.localityName
   csrReq.get_subject().organizationName = subject.organizationName
   csrReq.get_subject().organizationalUnitName = subject.organizationalUnitName
   csrReq.get_subject().emailAddress = subject.emailAddress
   csrReq.set_pubkey(key)
   csrReq.sign(key, "sha512")
   open(name + ".csr", "wb").write(
      crypto.dump_certificate_request(crypto.FILETYPE_PEM, csrReq))
   return (crypto.dump_certificate_request(crypto.FILETYPE_PEM, csrReq),
           crypto.dump_privatekey(crypto.FILETYPE_PEM, key))

def getCASignedCert(csr, caCert, caKey, name):
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
   signedCert.sign(caPKey, "sha512")
   open(name + ".crt", "wb").write(
      crypto.dump_certificate(crypto.FILETYPE_PEM, signedCert))
   open(name + ".key", "wb").write(
      crypto.dump_privatekey(crypto.FILETYPE_PEM, pub_key))
   return crypto.dump_certificate(crypto.FILETYPE_PEM, signedCert)

def generateSelfCertificate(sub, name):
   csr, csrKey = createCsr(getKey(), sub)
   return getSelfSignedCACert(csr, getKey(), name)

def generateCASignedCert(caKey, caCert, sub, name):
   csr, csrKey = createCsr(getKey(), sub)
   return getCASignedCert(csr, caCert, caKey, name)
