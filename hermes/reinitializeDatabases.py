#!/usr/bin/python3

#########################################################################
# Copyright 2018 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import argparse
import logging
import pprint
import util.json_helper
from util.product import Product
import yaml

log = None

def main():
   parser = argparse.ArgumentParser()
   parser.add_argument("--dockerComposeFile",
                       help="Accepts a docker compose file to start the DB.",
                       default=["../concord/docker/docker-compose.yml"])
   parser.add_argument("--logLevel",
                       help="Set the log level.  Valid values:"
                       "'DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL'",
                       default="INFO")

   args = parser.parse_args()
   setUpLogging(args)
   userConfig = util.json_helper.readJsonFile("resources/user_config.json")
   p = Product(args,
               None,
               userConfig)

   with open(args.dockerComposeFile[0], "r") as f:
      dockerCfg = yaml.load(f)
      pp = pprint.PrettyPrinter(indent=4)
      log.info("Database config:")
      pp.pprint(dockerCfg['services']['db-server'])
      log.info("Deleting old database files. You may get an error if you didn't use sudo!")
      p.clearDBsForDockerLaunch(dockerCfg)
      log.info("Launching the database from the docker file and running the sql. Please wait...")
      p.initializeHelenDockerDB(dockerCfg)
      log.info("Done")

def setUpLogging(args):
   '''
   Checks the log level passed in by the user and sets up logging.
   After running, get a logger and use it like this:

   log = logging.getLogger(__name__)
   log.info("Two things: {} {}".format("pen", "pencil"))
   '''
   stringLevel = args.logLevel.upper()

   try:
      intLevel = getattr(logging, stringLevel)
      logging.basicConfig(level=intLevel, format='%(message)s')
      global log
      log = logging.getLogger(__name__)
   except AttributeError:
      print("Log level", args.logLevel, "not valid.  See the help for valid",
            "values. Exiting")
      exit(1)

main()
