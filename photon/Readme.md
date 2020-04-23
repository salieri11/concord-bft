Process for updating vmbc-photon-3 image

Update Dockerfile with your changes.
Edit version.txt and increment the version number. Be sure to add a comment describing your change as well. Please follow Semver guidelines as much as possible. In short, increment:


MAJOR version when you make incompatible API changes,
MINOR version when you add functionality in a backwards-compatible manner, and
PATCH version when you make backwards-compatible bug fixes.


Get your changes reviewed and merge.
Trigger a Jenkins job [Link TBD] that will build the image, tag it with both MAJOR.MINOR.PATCH as well as MAJOR version (for e.g., vmbc-photon-3:1.2.3) and push the image to artifactory.
If a service is pointing to a base image vmbc-photon-3:1, it doesn't have to change that reference but will pick up the updated base image automatically the next time it builds.


Command to build

docker build . -f Dockerfile -t <label-name>

Date April 21st vmbc-photon-3:1.0.0
