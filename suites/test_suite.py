#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
from abc import ABC
from abc import abstractmethod

class TestSuite(ABC):
   @abstractmethod
   def getName(self): pass

   @abstractmethod
   def run(self): pass
