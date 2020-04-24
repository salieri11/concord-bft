#!/bin/bash -ex
# This script is used for building docker images.
# The docker directory for the image must have a Dockerfile, and a version.txt file
# of the form "IMAGE_VERSION=x.y".
#
# The image will be labeled with the following tags: x.y, x.y.build_number, and x.
# The image will be pushed to dev_repo

if [ $# -lt 4 ]
then
    echo "usage: $0
        <BUILD>
        <COMMIT_ID>
        <DOCKER_DIR>
        <IMAGE_NAME>"
    exit -1
else
    build_number="${1}"
    commit_id="${2}"
    dockerfile_dir="${3}"
    image_name="${4}"
fi

# import the version number in the form of major.minor.patch (patch can be omitted)
. ${dockerfile_dir}/version.txt
# pick up the just the major version
MAJOR_VERSION=${IMAGE_VERSION%%.*}

# create the destination images
# tag with image:major.minor
docker_major_minor_patch_image="athena-docker-local.artifactory.eng.vmware.com/${image_name}:${IMAGE_VERSION}"
# tag with image:major.minor.patch.build_number
docker_major_minor_patch_build_image="${docker_major_minor_patch_image}.${build_number}"
# tag with image:major
docker_major_image="athena-docker-local.artifactory.eng.vmware.com/${image_name}:${MAJOR_VERSION}"

# build the image, tagged with the version
docker  build \
        --pull \
        --build-arg build=${build_number} \
		--build-arg commitId=${commit_id} \
		. -f ${dockerfile_dir}"/Dockerfile" \
        -t ${docker_major_minor_patch_image}

# tag with the minor and major version
docker tag ${docker_major_minor_patch_image} ${docker_major_minor_patch_build_image}
docker tag ${docker_major_minor_patch_image} ${docker_major_image}
# push the image
docker push ${docker_major_minor_patch_image}
docker push ${docker_major_minor_patch_build_image}
docker push ${docker_major_image}

# delete the docker image from the local registry
docker rmi -f ${docker_major_minor_patch_image}
docker rmi -f ${docker_major_minor_patch_build_image}
docker rmi -f ${docker_major_image}