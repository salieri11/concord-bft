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
# TODO
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

FOLDER = "photon"
CONTENT_LIB_ARTIFACTORY = common.CONTENT_LIB_KEY + "/" + FOLDER + "/"
LIB_JSON_PATH = common.CONTENT_LIB_KEY + "/lib.json"
ITEMS_JSON_PATH = common.CONTENT_LIB_KEY + "/items.json"
ITEM_JSON_PATH = CONTENT_LIB_ARTIFACTORY + "item.json"


class s3_content_library(object):
    """
    This creates a content library on S3 based on manifest.json.
    This refers to make_vcsp_2018.py in
    https://code.vmware.com/samples/4388/automating-the-creation-of-3rd-party-content-library-directly-on-amazon-s3
i
    """

    def __init__(self, manifest_key):
        if manifest_key is None:
            raise ValueError("manifest must be present")

        self.manifest_key = manifest_key

        # create s3 helpers
        self.session = s3_common.setup_s3_session()
        self.bucket = s3_common.get_bucket(self.session, common.BUCKET_NAME)

        # get manifest file
        manifest_dict = s3_common.download_to_json(self.bucket,
                                self.manifest_key)

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
        lib_id = uuid.uuid4()
        creation = datetime.datetime.now()
        version = 1
        if last_libjson is not None:
            version = str(int(last_libjson.get("version")) + 1)

        return {
                "vcspVersion": "2",
                "version": str(version),
                "contentVersion": "1",
                "name": "concord-block",
                "id": "urn:uuid:%s" % lib_id,
                "created": creation.strftime("%Y-%m-%dT%H:%MZ"),
                "capabilities": {
                    "transferIn": ["httpGet"],
                    "transferOut": ["httpGet"],
                },
                "itemsHref": "items.json"
        }

    def make_item(self, directory, vcsp_type, name, files, description="",
                  properties={}, identifier=uuid.uuid4(),
                  creation=datetime.datetime.now(), version=2, library_id=""):
        '''
        add type adapter metadata for OVF template
        '''
        if "urn:uuid:" not in str(identifier):
            item_id = "urn:uuid:%s" % identifier
        else:
            item_id = identifier

        # generate sample type metadata for OVF
        # template so that subscriber can show OVF VM type
        type_metadata_value = {
                "id": item_id,
                "version": str(version),
                "libraryIdParent": library_id,
                "isVappTemplate": "false",
                "vmTemplate": "null",
                "vappTemplate": "null",
                "networks": [],
                "storagePolicyGroups": "null"}
        type_metadata = {
                    "key": "type-metadata",
                    "value": json.dumps(type_metadata_value),
                    "type": "String",
                    "domain": "SYSTEM",
                    "visibility": "READONLY"
        }
        return {
            "created": creation.strftime("%Y-%m-%dT%H:%MZ"),
            "description": description,
            "version": str(version),
            "files": files,
            "id": item_id,
            "name": name,
            "metadata": [type_metadata],
            "properties": properties,
            "selfHref": "%s/%s" % (FOLDER, "item.json"),
            "type": vcsp_type
        }

    def create_itemjson(self, lib_id):
        """
        creates data for
        - data for items.json that lies parallel to lib.json
          if is_items is True
        - data for item.json inside artifactory if is_items is False
        """
        items_json = {}
        files_items = []
        vcsp_type = None

        obj_list = s3_common.list_s3_objects(self.bucket,
                                             CONTENT_LIB_ARTIFACTORY)
        for obj in obj_list:
            file_name = relpath(obj.key, common.CONTENT_LIB_KEY)
            if ".ovf" in file_name:
                vcsp_type = "vcsp.ovf"
            if vcsp_type != "vcsp.ovf" and ".iso" not in file_name:
                vcsp_type = "vcsp.other"
            if vcsp_type not in ["vcsp.ovf", "vcsp.other"] and ".iso" in file_name:
                vcsp_type = "vcsp.iso"  # only if all files are iso, then it is ISO type

        for obj in obj_list:
            file_path = obj.key
            file_name = relpath(file_path, common.CONTENT_LIB_KEY)
            href = file_name
            if file_path == FOLDER or file_path.endswith("item.json"):
                continue
            size = obj.size
            last_modified = int(obj.last_modified.timestamp())
            file_json = {
                "name": file_name,
                "size": size,
                "etag": obj.e_tag.strip('"'),
                "generationNum": last_modified,
                "hrefs": [ href ]}

            if vcsp_type != "vcsp.ovf":
                raise ValueError("Fix me: non ovf is not supported")
            files_items.append(file_json)

        identifier = uuid.uuid4()
        items_json = self.make_item(FOLDER, vcsp_type, FOLDER, files_items, identifier=identifier, library_id=lib_id)

        return items_json


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

        # back up existing lib.json
        self.back_up_libjson(last_libjson)

        # copy manifest artifacts to content lib
        # TODO: we do not need to copy artifacts, the same folder should be used
        # To remove once lifecycle is properly implemented here
        self.copy_artifacts(self.bucket, CONTENT_LIB_ARTIFACTORY, self.artifactory)

        # create lib.json
        libjson_dict = self.create_libjson(last_libjson)

        # create item.json
        itemjson_dict = self.create_itemjson(libjson_dict["id"])

        # create items.json
        itemsjson_dict = {"items": [itemjson_dict]}

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
