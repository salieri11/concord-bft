"""!/usr/bin/python3
Prune unused consortiums and organisations from helendb

#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
"""

import argparse
import os
import psycopg2
import sys
from config import common
from lib import utils, helen

from psycopg2.extensions import ISOLATION_LEVEL_AUTOCOMMIT

logger = utils.setup_logging()

class HelenDB():
    def __init__(self, dbendpoint, user, password, database=None, port=5432):
        self.logger = utils.setup_logging()
        self.server = dbendpoint
        self.port =  port
        self.user = user
        self.password = password
        self.database = database
        self.connection = self.initialze_connection()
        self.logger.info("Initialized connection")

    def initialze_connection(self):
        if self.database is None:
            conn_string = "host='%s' user='%s' password='%s' port='%s'" % (
                        self.server, self.user, self.password, self.port)
        else:
            conn_string = "host='%s' dbname='%s' user='%s' password='%s' port='%s'" % (
                        self.server, self.database, self.user, self.password, self.port)
        con = psycopg2.connect(conn_string)
        con.set_isolation_level(ISOLATION_LEVEL_AUTOCOMMIT)
        return con

    def execute_query(self, query):
        try:
            cursor = self.connection.cursor()
            cursor.execute(query)
            rows = cursor.fetchall()
            return rows
        except Exception as e:
            self.logger.exception("Query %s failed" % (query, e))
            self.connection.rollback()

    def delete(self, query):
        try:
            cursor = self.connection.cursor()
            cursor.execute(query)
            self.connection.commit()
        except Exception as e:
            self.logger.exception("Delete %s failed" % (query, e))
            self.connection.rollback()

    def clear_consortium(self, consortium_id):
        #get blockchain ids from consortium_id
        query = ("select row_key, column_name from entity where \
            column_name = 'helen.blockchain' AND row_key \
            in (select to_row from link where from_row = '%s')" % consortium_id)
        data = self.execute_query(query)
        for bcinfo in data:
            blockchain_id = bcinfo[0]
            print(blockchain_id)
            self.delete("delete from entity where row_key in \
                    (select to_row from link where from_row = '%s')" %
                    blockchain_id)
            self.delete("delete from link where from_row = '%s'" % blockchain_id)
        self.delete("delete from entity where column_name = 'helen.blockchain' \
                    AND row_key in (select to_row from link where from_row = '%s')"
                    % consortium_id)
        self.delete("delete from link where from_row = '%s'" % consortium_id)
        self.delete("delete from link where to_row = '%s'" % consortium_id)
        self.delete("delete from entity where row_key = '%s'" % consortium_id)

    def __del__(self):
        try:
            self.connection.close()
        except Exception as e:
            self.logger.error("Error closing connection %s" % e)


def setup_arguments():
    """
        Arg setup
    """
    parser = argparse.ArgumentParser(description=
                            "Remove unused consortium data from helendb")
    parser.add_argument("--env", type=str, required=True,
                        choices=["production", "staging"], help="db env to prune")
    parser.add_argument("--blockchainids", required=True, nargs="*", type=str,
                        help="Blockchain id to delete")
    parser.add_argument("--helenurl", type=str, required=True,
                    help="Helen url for environment")
    args = parser.parse_args()
    print(args)
    return args

if __name__ == "__main__":
    args = setup_arguments()
    data = utils.get_vault_constants('helendb')
    env =  args.env
    db = HelenDB(data[env]['server'], data[env]['user'],
                data[env]['password'], data[env]["database"],
                data[env]['port'])
    hapi = helen.HelenApi(args.helenurl, csp_env=env)
    for bcid in args.blockchainids:
        cinfo = hapi.get_consortium_from_blockchain(bcid)
        logger.info("Clearing blockchain %s and consortium %s "\
                    "record from helen" % (bcid, cinfo["consortium_name"]))
        db.clear_consortium(cinfo["consortium_id"])
