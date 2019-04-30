#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import time

hexIndicator = "0x"

def trimHexIndicator(s):
   '''
   Remove the leading "0x".
   '''


   if s.startswith(hexIndicator):
      return s[len(hexIndicator):]
   else:
      return s

def decToEvenHex(d):
   '''
   Convert a decimal number to a hex string, and with an even number of
   digits.
   '''
   hexString = hex(d)

   if not len(hexString) % 2 == 0:
      hexString = hexString[0:len(hexIndicator)] + "0" + hexString[len(hexIndicator):]

   return hexString

def decToEvenHexNo0x(d):
   '''
   Convert a decimal number to a hex string, without the leading "0x",
   and with an even number of digits.
   '''
   hexString = decToEvenHex(d)
   return trimHexIndicator(hexString)

def stringOnlyContains(checkMe, allowed):
   '''
   Given a string and a string of characters it may contain, returns whether
   the string only contains the characters it may.
   '''
   for c in checkMe:
      if not c in allowed:
         return False

   return True

def epochToLegible(t):
   '''
   Given an epoch time number, return a human readable string.
   '''
   return time.strftime('%Y-%m-%d %H:%M:%S', time.gmtime(t))
