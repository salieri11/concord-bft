import boto3
from util import hermes_logging

log = hermes_logging.getMainLogger()


class S3Helper:
    '''
    S3 object store helper class
    '''
    def __init__(self, url, access_key, secret_key, region='us-east-1'):
        '''
        Create a connection to an S3 object store

        Args:
            url (string): endpoint url in the format http(s)://host:port
            access_key (string): access key (username) for the object store
            secret_key (string): secret key (password) for the object store
            region (string): region where the object store is located
        '''
        self.res = boto3.resource('s3',
                                  endpoint_url=url,
                                  aws_access_key_id=access_key,
                                  aws_secret_access_key=secret_key,
                                  region_name=region)

        # Invoked to verify validity of connection
        buckets = self.buckets()
        log.info("Successfully connected to S3 object store {}".format(url))
        log.info("Available buckets: {}".format(buckets))

    def buckets(self):
        '''
        List the available buckets

        Returns:
            list(string): a list of available bucket names
        '''
        return [bucket.name for bucket in self.res.buckets.all()]

    def make_bucket(self, bucket):
        '''
        Create a bucket if it does not already exist

        Args:
            bucket (string): name of bucket to be created
        '''
        buckets = self.buckets()
        if bucket in buckets:
            log.info("Can not create: bucket {} already exists".format(bucket))
            return

        self.res.Bucket(bucket).create()
        log.info("Created bucket {}".format(bucket))

    def remove_bucket(self, bucket):
        '''
        Delete a bucket if it exists

        Args:
            bucket (string): name of bucket to be deleted
        '''
        buckets = self.buckets()
        if bucket not in buckets:
            log.info("Can not delete: bucket {} does not exist".format(bucket))
            return

        self.res.Bucket(bucket).delete()
        log.info("Deleted empty bucket {}".format(bucket))

    def list_bucket(self, bucket, prefix=None):
        '''
        List the contents of an existing bucket

        Args:
            bucket (string): name of bucket to be inspected
            prefix (string): case sensitive prefix for filtering object names
        Returns:
            list(string): a list of available object names
        '''
        if prefix:
            return [obj.key for obj in self.res.Bucket(bucket).objects.filter(Prefix=prefix)]

        return [obj.key for obj in self.res.Bucket(bucket).objects.all()]

    def clear_bucket(self, bucket):
        '''
        Remove all objects from an existing bucket

        Args:
            bucket (string): name of bucket to be emptied
        '''
        for idx, obj in enumerate(self.res.Bucket(bucket).objects.all(), start=1):
            obj.delete()

        log.info("Deleted {} objects from bucket {}".format(idx, bucket))

    def upload(self, bucket, path_local, path_remote):
        '''
        Upload a file to the object store

        Args:
            bucket (string): name of bucket to be used
            path_local (string): path to local file
            path_remote (string): remote location of the object
        '''
        self.res.Bucket(bucket).upload_file(path_local, path_remote)
        log.info("Uploaded {} to {}/{}".format(path_local, bucket, path_remote))


    def download(self, bucket, path_local, path_remote):
        '''
        Download a file from the object store

        Args:
            bucket (string): name of bucket to be used
            path_local (string): path to local file
            path_remote (string): remote location of the object
        '''
        self.res.Bucket(bucket).download_file(path_remote, path_local)
        log.info("Downloaded {}/{} to {}".format(bucket, path_remote, path_local))

    def delete(self, bucket, path_remote):
        '''
        Delete an object from the object store

        Args:
            bucket (string): name of bucket containing the object
            path_remote (string): remote location of the object
        '''
        self.res.Object(bucket, path_remote).delete()
        log.info("Deleted object {}/{}".format(bucket, path_remote))

