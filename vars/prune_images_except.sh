############################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
############################################################################

# output all images for deletion
echo "Images to be deleted:"
docker images --format "{{.ID}}    {{.Tag}}    {{.Repository}}" | grep "0.0.0.*" | grep -Eiv "$1"

echo "Unnamed images to be deleted:"
docker images --format "{{.ID}}    {{.Tag}}    {{.Repository}}" | grep "<none>"

# get all image ids matching the pattern
image_ids=$(docker images --format "{{.ID}} {{.Tag}}" | grep "0.0.0.*" | grep -Eiv "$1" | cut -d " " -f 1)
for image_id in $image_ids
do
  docker rmi -f "$image_id"
done

# remove <none>:<none> unnamed local images
unnamed_image_ids=$(docker images --format "{{.ID}} {{.Tag}}" | grep "<none>" | cut -d " " -f 1)
for image_id in $unnamed_image_ids
do
  docker rmi -f "$image_id"
done
