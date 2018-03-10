#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import argparse
import tempfile
import time

from . import test_suite
from util.product import Product

PRODUCT_CONFIG_JSON = "resources/product_launch_config.json"

class CoreVMTests(test_suite.TestSuite):
   _args = None

   def __init__(self, passedArgs):
      self._args = passedArgs

   def getName(self):
      return "CoreVMTests"

   def run(self):
      p = Product(self._args.resultsDir)
      p.launchProduct(PRODUCT_CONFIG_JSON)

      print("The product is running.  Do a few curls to simulate tests.")
      time.sleep(10)
      print("Tests are done.")

      p.stopProduct()

      # Should be a file.
      return {
         "results": "foo"
      }
