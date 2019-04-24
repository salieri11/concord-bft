#!/usr/bin/python3

#########################################################################

# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential

#########################################################################


import json
import uuid
import os
import datetime
import argparse
from os.path import relpath
from urllib.parse import urlparse

import s3_common
import common

#######################################################################
#TODO: 
# - logging
# - exception handling and bool return

# Below could be done in future or as part of this
# - manifest.json is pulled from S3 currently. Could be changed to take
# from local (in memory json pass)
# - creation of lib.json and specifically items.json is not generic, it
# is hard coded to the structure we currently hold. This should be made
# generic, reading from the content lib structure
# - copy to content lib folder needs to be recursive unlike now
# - backup only backs up the lib.json. Needs rework depending on requirement
########################################################################

CONTENT_LIB_ARTIFACTORY = common.CONTENT_LIB_KEY + "/photon/"
LIB_JSON_PATH = common.CONTENT_LIB_KEY + "/lib.json"
ITEMS_JSON_PATH = common.CONTENT_LIB_KEY + "/items.json"
ITEM_JSON_PATH = CONTENT_LIB_ARTIFACTORY + "item.json"

class s3_content_library(object):
    """
    This creates a content library on S3 based on manifest.json
    """

    def __init__(self, manifest_key):
        if manifest_key is None:
            raise ValueError("manifest must be present")

        self.manifest_key = manifest_key

        # create s3 helpers
        self.session = s3_common.setup_s3_session()
        self.bucket = s3_common.get_bucket(self.session, common.BUCKET_NAME)

        # get manifest file
        manifest_dict = s3_common.download_to_json(self.bucket, self.manifest_key)

        # get artifactory key
        s3_url = manifest_dict.get('os_template_path')
        urlp = urlparse(s3_url)

        self.artifactory = urlp.path.lstrip("/")


    def get_last_libjson(self):
        """
        gets the last version of libjson
        """
        if s3_common.exists(self.bucket, LIB_JSON_PATH):
            libjson = s3_common.download_to_json(self.bucket, LIB_JSON_PATH)
            return libjson
        else:
            return None


    def create_libjson(self, last_libjson=None):
        """
        creates the lib.json file, returns dict
        """
        version = 1
        gen_id = str(uuid.uuid4())

        if last_libjson is not None:
            version = str(int(last_libjson.get("version")) + 1)
            gen_id = last_libjson.get("id")

        lib_dict = { "version": version,
                "created": datetime.datetime.now().isoformat(),
                "id": gen_id,
                "vcspVersion": "2", # keeping default
                "name": "some name change me",
                "description": "fill me later",
                "itemsHref": "items.json", #TODO
                "selfHref": "lib.json",
                "metadata": [],
                "contentVersion": "1"}
        return lib_dict


    def create_itemsjson(self, is_items=False):
        """
        creates data for
        - data for items.json that lies parallel to lib.json if is_items is True
        - data for item.json inside artifactory if is_items is False
        """
        gen_id = str(uuid.uuid4())
        version = "1"
        contentVersion = "2"
        description = "fill something interesting"
        created = datetime.datetime.now().isoformat()
        typ = "vcsp.ovf"
        name = "name-me"

        last_itemsjson = s3_common.download_to_json(self.bucket, ITEMS_JSON_PATH)
        if last_itemsjson is not None:
            gen_id = last_itemsjson.get("id")
            version = str(int(last_itemsjson.get("version")) + 1)

        # create master dict
        items_dict = { "version": version,
                "contentVersion": contentVersion,
                "description": description,
                "created": created,
                "selfHref": ITEM_JSON_PATH,
                "type": typ,
                "id": gen_id,
                "name": name}

        # create per item dict
        item_dict = {}

        obj_list = s3_common.list_s3_objects(self.bucket, CONTENT_LIB_ARTIFACTORY)
        for ind in range(len(obj_list)):
            obj = obj_list[ind]
            obj_dict = { "name": os.path.basename(obj.key),
                    "size": obj.size,
                    "hrefs": {"0": relpath(obj.key, common.CONTENT_LIB_KEY)},
                    "srefs": "null",
                    "etag": "1",
                    "generationNum": str(int(obj.last_modified.timestamp())) }
            item_dict.update({ind: obj_dict})

        # TODO: This is currently hard coded according to structure we need
        # This could be made generic in future if needed with minor tweak
        if is_items:
            items_dict.update({
                "0": {"files": item_dict}})
        else:
            items_dict.update({
                "files": item_dict})

        return items_dict


    def back_up_libjson(self, last_libjson):
        """
        versions lib.json and backs up in S3
        """
        # return if this is the first ever lib.json
        if last_libjson is None:
            return

        # check if back up key exists else create
        if not s3_common.exists(self.bucket, common.BACKUP_KEY):
            s3_common.create_folder(self.bucket, common.BACKUP_KEY)

        # version lib.json and back up
        version = last_libjson.get("version")
        s3_common.upload_to_json(self.bucket, "%s/lib-%s.json" % (common.BACKUP_KEY, version), last_libjson)


    def copy_artifacts(self, bucket, to_key, from_key):
        """
        copies artifactory to content lib
        """
        # It is better to delete and recreate rather than check for existence
        s3_common.delete_folder(self.bucket, to_key)
        s3_common.create_folder(self.bucket, to_key)

        # copy artifactory
        s3_common.copy_elements(self.bucket, from_key, to_key)


    def create_content_lib(self):
        """
        - creates the content library on s3
        - keeps backup if older lib.json
        """

        # get lib json version
        last_libjson = self.get_last_libjson()

        # create lib.json
        libjson_dict = self.create_libjson(last_libjson)

        # back up existing lib.json
        self.back_up_libjson(last_libjson)

        # create items.json
        itemsjson_dict = self.create_itemsjson(is_items=True)

        # create item.json
        itemjson_dict = self.create_itemsjson()

        # copy manifest artifacts to content lib
        self.copy_artifacts(self.bucket, CONTENT_LIB_ARTIFACTORY, self.artifactory)

        # upload lib.json to content lib
        s3_common.upload_to_json(self.bucket, LIB_JSON_PATH, libjson_dict)

        # upload items.json
        s3_common.upload_to_json(self.bucket, ITEMS_JSON_PATH, itemsjson_dict)

        # upload item.json
        s3_common.upload_to_json(self.bucket, ITEM_JSON_PATH, itemjson_dict)


def setup_arguments():
    startTime = datetime.datetime.now()
    parser = argparse.ArgumentParser(description=
            "Get created manifest from S3 and create/update content library on S3")
    parser.add_argument("--manifest", default=None,
            help="The S3 Manifest key/folder and file")

    args = parser.parse_args()
    return args

if __name__ == "__main__":
    args = setup_arguments()
    content_lib = s3_content_library(args.manifest)
    content_lib.create_content_lib()

