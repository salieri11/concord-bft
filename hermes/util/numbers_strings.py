#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import random
import string
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

def decToInt256HexNo0x(d):
   '''
   Convert a decimal number into a solidity int256 (32 bytes) without
   the leading "0x".  e.g. For passing to a solidity contract constructor.
   '''
   hexString = trimHexIndicator(hex(d))
   zeroesNeeded = 64 - len(hexString)
   return ("0" * zeroesNeeded) + hexString

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

def randomGpsPoint():
   '''
   Returns a tuple of a randomly generated (latitude, longitude)
   Latitude: -90 to +90
   Longitude: -180 to +180
   Decimal: Up to six digits.
   '''
   latInt = random.randrange(-90, 90)
   latDec = random.randrange(0, 999999)
   latitude = float("{}.{}".format(latInt,latDec))

   lonInt = random.randrange(-180, 180)
   lonDec = random.randrange(1, 999999)
   longitude = float("{}.{}".format(lonInt,lonDec))

   return (latitude, longitude)

def randomIP4Address():
   '''
   Returns a random IP4 address.
   '''
   return "{}.{}.{}.{}".format(random.randrange(0, 255),
                               random.randrange(0, 255),
                               random.randrange(0, 255),
                               random.randrange(0, 255))

def to_signed_int(value: int, bits: int = 64) -> int:
   '''
   Interpret an integer input as an unsigned value of specified bit precision,
   and return the byte-content equivalent signed value
   :param value: int input
   :param bits: bit precision to use to interpret the input value as unsigned value
   :return: value reinterpreted as signed integer of specified bit precision
   '''
   mask = (1 << bits) - 1
   return (value | ~mask) if value & (1 << (bits - 1)) else (value & mask)


def random_string_generator(size=6, chars=string.ascii_uppercase + string.digits, mustNotMatch=None):
   while True:
      ret = ''.join(random.choice(chars) for _ in range(size))

      if ret != mustNotMatch:
         return ret

