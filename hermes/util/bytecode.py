#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
def getPushInstruction(numberToPush):
   '''
   Given an integer (e.g. not a hex string or snippet of bytecode), make sure we
   use the right PUSHX instruction for the given number.
   96  = 0x60 = PUSH1 = uint8  (number up to 255)
   97  = 0x61 = PUSH2 = uint16 (number up to 65,535)
   98  = 0x62 = PUSH3 = uint24 (number up to 16,777,215)
   ...
   127 = 0x7F = PUSH32 = uint256

   Returns the hex instruction.
   '''
   pushInstruction = 96

   for i in range(1, 32):
      maxNumSupportedByThisPush = 2 ** (i*8) - 1
      if numberToPush <= maxNumSupportedByThisPush:
         break
      else:
         pushInstruction += 1

   return hex(pushInstruction)

def addBytePadding(s, byteBoundary):
   '''
   Pad a string, s, to a multiple of byteBoundary bytes by prepending zeroes.
   '''
   charBoundary = byteBoundary*2

   if s:
      if len(s) % charBoundary == 0:
         return s
      else:
         desiredLength = len(s) + charBoundary - (len(s)%(charBoundary))
         return (desiredLength-len(s))*"0" + s
   else:
      return byteBoundary*2*"0"
