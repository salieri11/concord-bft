#########################################################################

# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential

#########################################################################

from boto3.session import Session
import json
import common

# TODO: setup logging

def setup_s3_session():
    session = Session(aws_access_key_id=common.AWS_ACCESS_KEY_ID,
            aws_secret_access_key=common.AWS_SECRET_ACCESS_KEY)
    return session

def get_bucket(session, bucket_name):
    """
    gets the bucket instance from S3 resource
    """
    #TODO: Exception handling
    s3 = session.resource('s3')
    return s3.Bucket(bucket_name)

def create_folder(s3_bucket, folder_name):
    """
    creates specified folder under given bucket in S3
    """
    # TODO: Exception handling
    s3_bucket.put_object(Key=folder_name)

def delete_folder(s3_bucket, folder_name):
    """
    deletes specified folder and its contents
    """
    #TODO: Exception handling
    s3_bucket.objects.filter(Prefix=folder_name).delete()

def list_s3_objects(s3_bucket, folder):
    """
    lists all keys in given folder/key in a bucket
    returns a list of s3 objects
    """
    # TODO: Exception handling
    obj_list = []
    for obj in s3_bucket.objects.filter(Prefix=folder):
        if obj.key == folder:
            continue
        obj_list.append(obj)
    return obj_list

def copy_elements(s3_bucket, copy_from, copy_to):
    """
    copies all files from one folder to another in the given bucket
    """
    #TODO: Exception handling
    for obj in s3_bucket.objects.filter(Prefix=copy_from):
        if obj.key == copy_from:
            continue
        from_src = { 'Bucket': s3_bucket.name,
                'Key': obj.key}
        new_key = obj.key.replace(copy_from, copy_to)
        new_obj = s3_bucket.Object(new_key)
        new_obj.copy(from_src)

def copy_single_element(s3_bucket, from_file, to_file):
    """
    copy single element from one folder to another in a given bucket
    """
    #TODO: Exception handling
    copy_src = {
            'Bucket': s3_bucket.name,
            'Key': from_file}

    s3_bucket.copy(copy_src, to_file)

def exists(s3_bucket, folder):
    """
    Checks if the folder exists in given bucket
    """
    #TODO: Exception handling
    objs = list(s3_bucket.objects.filter(Prefix=folder))
    if len(objs) > 0 and objs[0].key == folder:
        return True
    return False

def download_to_json(s3_bucket, filepath):
    """
    downloads a json file to a json object
    """
    # TODO: specific Exception handling
    # only handling done now is optimistically assuming
    # no S3 errors other that does not exist
    try:
        obj = s3_bucket.Object(filepath)
        data = obj.get()['Body'].read()
        json_content = json.loads(data)
        return json_content
    except Exception:
        # TODO: different exception type and handling
        return None

def upload_to_json(s3_bucket, filepath, json_data):
    """
    uploads json element to given filepath/key in S3
    """
    # TODO: Exception handling
    s3_bucket.Object(filepath).put(Body=json.dumps(json_data, indent=2))
