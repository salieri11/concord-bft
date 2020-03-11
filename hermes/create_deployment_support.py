#########################################################################
# Copyright 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

# CLI version (standalone) of utility file to create deployment support bundle
# for the passed in concord IPs
#
# Execute this file with the following options:
#    --replicas <Comma separated list of concord node/replica IPs>
#    --replicaType <"daml"/"ethereum">
#    --saveTo <deployment support archive path>

import argparse
import util.helper
import logging

logging.basicConfig(level=logging.INFO, format='%(asctime)s %(levelname)s %(message)s',
                          datefmt='%Y-%m-%d %H:%M:%S')
log = logging.getLogger("main")

if __name__ == '__main__':
   parser = argparse.ArgumentParser()
   parser.add_argument("--replicas", required=True,
                       help="Comma separated list of concord node/replica IPs")
   parser.add_argument("--replicaType", required=True,
                       help="concord node/replica Type (ethereum/daml/etc)")
   parser.add_argument("--saveTo", required=True,
                       help="deployment support bundle archive path")
   args = parser.parse_args()

   util.helper.create_concord_support_bundle(args.replicas.split(','),
                                             args.replicaType, args.saveTo)
