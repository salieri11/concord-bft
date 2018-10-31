#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
def trimHexIndicator(s):
   '''
   Remove the leading "0x".
   '''
   hexIndicator = "0x"

   if s.startswith(hexIndicator):
      return s[len(hexIndicator):]
   else:
      return s

def decToEvenHexNo0x(d):
   '''
   Convert a decimal number to a hex string, without the leading "0x",
   and with an even number of digits.
   '''
   hexString = hex(d)
   hexString = trimHexIndicator(hexString)

   if not len(hexString) % 2 == 0:
      hexString = "0" + hexString

   return hexString

def stringOnlyContains(checkMe, allowed):
   '''
   Given a string and a string of characters it may contain, returns whether
   the string only contains the characters it may.
   '''
   for c in checkMe:
      if not c in allowed:
         return False

   return True
