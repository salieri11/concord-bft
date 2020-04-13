"""!/usr/bin/python3
Script  to push artifacts from Artifactory to Bintray(production repo)

#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
"""

import argparse
import os
import sys
from config import common, cdn
from lib import image_utils
from lib import utils

logger = utils.setup_logging()

def setup_arguments():
    components = [val for vals in
                    common.BC_COMPONENTS.values() for val in vals]
    parser = argparse.ArgumentParser(description=
                    "Upload blockchain artifacts to production repo and "
                    "publish env file")
    parser.add_argument("--components", choices=components, default=components,
                    nargs='*', help="Blockchain components"
                    " to update in production repository")
    parser.add_argument("--internaltag", default="latest",
                    help="Tag to use to query internal artifacts to publish")
    parser.add_argument("--externaltag", default="latest",
                    help="Tag to use for external artifacts")
    parser.add_argument("--envpath", default=os.getcwd(),
                    help="Path to dump component env info for given "
                    "externaltag")
    parser.add_argument("--externalrepo", default=cdn.BINTRAY_REPO_URL,
                    help="External repo to publist images to")
    parser.add_argument("--externalrepons", default=None,
                    help="External repo namespace to push images to")
    parser.add_argument("--force", action="store_true", default=False,
                    help="If set, force update of version if already present")
    args = parser.parse_args()
    return args

if __name__ == "__main__":
    args = setup_arguments()
    logger.info(args)
    image_manager = image_utils.ImageManager()
    if args.force is False:
        components = image_manager.check_if_version_exists(args.externaltag,
                                                namespace=args.externalrepons,
                                                components=args.components)
        if len(components) == 0:
            logger.info("Version %s of input components %s"
                        " already exist in namespace %s" % (args.externaltag,
                                                        args.components,
                                                        args.externalrepons))
            sys.exit(0)
    else:
        components = args.components
    logger.info("%s components require version update" % " ".join(components))
    artifacts = image_manager.find_artifactory_metadata(
                                            components=components,
                                            tag=args.internaltag)
    try:
        internal_artifacts = image_manager.pull_from_artifactory(tags=artifacts)
        logger.info(internal_artifacts)
        tags = image_manager.tag_images(internal_artifacts, tag=args.externaltag,
                                        repourl=args.externalrepo,
                                        namespace=args.externalrepons)
        logger.info("Tagged blockchain images with %s" % args.externaltag)
        logger.info(tags)
        if image_manager.push_to_artifactory(tags) is True:
            logger.info("Successfully pushed artifacts to production")
            logger.info("Publishing docker image  env to %s" % args.envpath)
            artifact_path = "%s/artifacts.env" % (args.envpath)
            logger.info("Publishing docker image  env to %s" % artifact_path)
            image_manager.generate_bintray_image_env(components,
                            namespace=args.externalrepons,
                            version=args.externaltag,
                            outfile=artifact_path)
    except Exception as e:
        logger.exception("Error uploading artifacts %s" % e)
        sys.exit(1)
    finally:
        image_manager.delete_images(all_images=True)
        logger.info("Cleaned up images on local env")

