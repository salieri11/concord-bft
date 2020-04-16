"""
    Docker image management utilities abstracted in
    ImageManager class.
    Supports bintray and artifactory image utilites
"""
import json
import requests
import docker
from bintray.bintray import Bintray
from config import common, cdn
from lib import utils


class ImageManager():
    """
        Image manager provides umbrella utilities over
        docker images built for blockchain components.
    """
    _bintray_user = None
    _bintray_password = None
    _artifactory_user = None
    _artifactory_key = None
    _artifactory_repo = None

    def __init__(self, components=None):
        """
            Initialize class with bintray and docker client
            components: Dict seperating saas and blockchain components
            {
                "saas": [c1,c2],
                "blockchain": [c3,c4],
            }
        """
        self.artifactory_api = None
        # arbitary timeout based on emprical usage
        self.docker_client = docker.from_env(timeout=240)
        self._load_secrets()
        self.bintray = Bintray(username=self._bintray_user,
                          api_key=self._bintray_password)
        if components is None:
            self.components = common.BC_COMPONENTS
        else:
            self.components = components
        self.logger = utils.setup_logging()

    def _load_secrets(self):
        artifactory_data = utils.get_vault_constants("artifactory")
        bintray_data = utils.get_vault_constants("bintray")
        self._bintray_user = bintray_data["user"]
        self._bintray_password = bintray_data["key"]
        self._artifactory_user = artifactory_data["username"]
        self._artifactory_key = artifactory_data["api_key"]
        self._artifactory_repo = artifactory_data["default_repo"]
        self.artifactory_api = "%s/artifactory/api" % (artifactory_data["url"])

    def add_artifactory_image_property(self, image, property_dict,
                                       tag="latest"):
        """
            Add property to artifactory image
            property_dict single key value dict in the format
            {"name": "NAME"
            "value": "VALUE"}
        """
        properties = {"property":
                      {"name": property_dict["name"]},
                      "selectedValues": [property_dict["value"]]
                     }
        path = "%s/%s" % (image, tag)
        url = ("%s/artifactproperties?path=%s&repoKey=%s" %
                (self.artifactory_api, path, self._artifactory_repo))
        artifactory_auth = (self._artifactory_user, self._artifactory_key)
        req = requests.post(url, auth=artifactory_auth,
                            json={"property": properties})
        if req.status_code == 201:
            self.logger.info("Successfully created properties for image %s" %
                             path)
        else:
            self.logger.error("Failed to create properties %s" % req.text)

    def get_artifactory_image_property(self, image, tag="latest"):
        """
            Get artifactory image property
        """
        path = "%s/%s" % (image, tag)
        url = ("%s/artifactproperties?path=%s&repoKey=%s" %
                (self.artifactory_api, path, self._artifactory_repo))
        artifactory_auth = (self._artifactory_user, self._artifactory_key)
        req = requests.get(url, auth=artifactory_auth)
        if req.status_code == 200:
            return req.json()
        else:
            self.logger.error("Failed to get artifactory image property %s" %
                              req.text)
            return None

    def get_bintray_image_property(self, improperty, package,
                                version="latest", **kwargs):
        """
            Get image property for a version of image,
            returns None if not found
            improperty: image label/property
            version: version/tag
            package: package/docker image
        """
        repo = kwargs.get("repo", cdn.BINTRAY_REPO)
        subject = kwargs.get("subject", cdn.BINTRAY_SUBJECT)
        data = self.bintray.get_version(subject=subject, repo=repo,
                                    package=package, version=version)
        return data["attributes"].get(improperty, None)

    def generate_bintray_image_env(self, packages, namespace=None,
                                 version="latest", outfile=None, **kwargs):
        """
            Generate images env list from bintray
            subject: bintray domain, "vmware" for now
            repo: bintray repo
            packages: list of packages (docker images)
            version: docker tag to source
            outfile: Filepath to dump an env file of images
        """
        version_list = []
        for pkg in packages:
            if namespace is not None:
                pkg = ("%s:%s" % (namespace, pkg))
            build = self.get_bintray_image_property(cdn.BINTRAY_BUILD_LABEL,
                                    version=version, package=pkg, **kwargs)
            if isinstance(build, list) is True:
                version_list.append("%s=%s" % (pkg, build[0]))
            else:
                version_list.append("%s=%s" % (pkg, build))
        if outfile is not None:
            with open(outfile, "w") as filehandle:
                filehandle.write("\n".join(version_list))
            self.logger.info("Successfully dumped release version to file")
        else:
            self.logger.info("\n".join(version_list))

    def build_image(self, path, dockerfile, tag):
        """
            Build image given tags
            path to root dir of where the build needs to invoked
            dockerfile - abs path to dockerfile
            tag - provide tag name for the built image
        """
        try:
            image, _ = self.docker_client.images.build(
                                path=path, dockerfile=dockerfile, tag=tag)
            self.logger.info("successfully built image with tag %s and id" %
                            (image.tags, image.short_id.split(":")[1]))
            return image.short_id
        except Exception as e:
            self.logger.error("Error building %s %s" % (dockerfile, e))
            return None

    def find_artifactory_metadata(self, components=[],
                                  component_type="all", tag="latest"):
        """
            Populate a dict with given component list
            as keys and value being latest artifact
            tag
            components: list of components to update, if not empty
                       updates only specific components
            component_type: saas or blockchain
            tag: image tag to search
        """
        artifacts = []
        tag_dict = {}
        if len(components) == 0:
            if component_type == "all":
                for _, val in self.components.items():
                    artifacts.extend(val)
            else:
                artifacts = self.components[component_type]
        else:
            artifacts = components
        tag_dict = dict.fromkeys(artifacts, None)
        artifactory_auth = (self._artifactory_user, self._artifactory_key)
        for artifact in artifacts:
            if tag == "latest":
                url = ("%s/storage/%s/%s?lastModified"
                            % (self.artifactory_api,
                            self._artifactory_repo, artifact))
            else:
                url = ("%s/storage/%s/%s/%s/manifest.json"
                           % (self.artifactory_api,
                            self._artifactory_repo, artifact, tag))
            self.logger.info(url)
            req = requests.get(url,
                            auth=artifactory_auth)
            if req.status_code == 200:
                rdict = json.loads(req.text)
                tag_dict[artifact] = rdict["uri"].split("/")[-2]
            else:
                self.logger.error("Unable to retrieve manifest for "
                                  "product %s error %s"
                                  % (artifact, req.text))
        return tag_dict

    def pull_from_artifactory(self, tags, repo=None):
        """
            Pull images from artifactory to local docker env
            client - docker client
            tags - dict of artficats and correspoding tags
            repo - full path to repo (registry/repo)
            Return
            - list of repo/images
        """
        artifact_full_list = []
        try:
            for image in tags:
                self.logger.info("Pulling image %s from artifactory" % image)
                if repo:
                    repo_url = ("%s.artifactory.eng.vmware.com" % (repo))
                else:
                    repo_url = ("%s.artifactory.eng.vmware.com" %
                                (self._artifactory_repo))
                image = self.docker_client.images.pull("%s/%s:%s" %
                        (repo_url, image, tags[image]))
                artifact_full_list.extend(image.tags)
            return artifact_full_list
        except Exception as e:
            self.logger.error("Error pulling artifact from repo %s" % e)
            return None

    def tag_images(self, from_ims, tag="latest", repourl=None, namespace=None):
        """
            Tag existing images with tags based on input list
            of images
            ["image:tag", "image2:tag" ...]
        """
        external_artifact_list = []
        for image in from_ims:
            cimg = self.docker_client.images.get(image)
            if namespace is not None:
                repopath = ("%s/%s/%s:%s" % (repourl, namespace,
                                        image.split("/")[1].split(":")[0], tag))
            else:
                repopath = ("%s/%s:%s" % (repourl,
                                        image.split("/")[1].split(":")[0], tag))
            cimg.tag(repopath)
            external_artifact_list.append(repopath)
        return external_artifact_list

    def tag_image(self, cname, nname, ctag="latest", ntag="latest"):
        """
            Tag existing image with new tag,
            cname - image name without tag
            nname - new name of image
            ctag - current tag name, default to latest
            ntag - new tag, default to latest
        """
        try:
            cname = cname.split(":")[0]
            cimg = self.docker_client.images.get("%s:%s" % (cname, ctag))
            cimg.tag("%s:%s" % (nname, ntag))
            return True
        except Exception as e:
            self.logger.error("Unable to tag image, check if image exists: %s"
                             % e)
            return False

    def push_to_artifactory(self, artifactlist):
        """
            Push images to artifactory based on input list
            ["image:tag", "image2:tag" ...]
            Returns:
            - True if all pushes are successful
              False if there is a failure
        """
        for artifact in artifactlist:
            push_gen = self.docker_client.images.push(artifact, stream=True,
                                                      decode=True)
            for upload in push_gen:
                self.logger.info(upload)
            if "error" in upload:
                self.logger.error("Error uploading image %s" % upload["error"])
                return False
        return True

    def delete_images(self, artifactlist=[], all_images=False):
        """
            Delete images in docker env
        """
        self.docker_client.images.prune()
        if all_images is True:
            for image in self.docker_client.images.list():
                self.docker_client.images.remove(image.short_id, force=True)
            return True
        try:
            for artifact in artifactlist:
                self.docker_client.images.remove(artifact, force=True)
            return True
        except Exception as e:
            self.logger.error("Error deleting image %s" % e)
            return False

    def  delete_bintray_version(self, tag,
                            namespace="vmwblockchain",
                            components=[], **kwargs):
        """
            Delete bintray image version based on tag
        """
        repo = kwargs.get("repo", cdn.BINTRAY_REPO)
        subject = kwargs.get("subject", cdn.BINTRAY_SUBJECT)
        if len(components) == 0:
            for _, val in self.components.items():
                components.extend(val)
        for component in components:
            try:
                pkg = ("%s:%s" % (namespace, component))
                ret = self.bintray.delete_version(subject, repo, pkg, tag)
            except Exception as e:
                self.logger.exception("Error deleting image version %s %s" %
                                    (pkg, e))
                continue
        self.logger.info("Successfully deleted images with version %s" % tag)

    def check_if_version_exists(self, tag, namespace="vmwblockchain",
                            components=[], **kwargs):
        repo = kwargs.get("repo", cdn.BINTRAY_REPO)
        subject = kwargs.get("subject", cdn.BINTRAY_SUBJECT)
        if len(components) == 0:
            for _, val in self.components.items():
                components.extend(val)
        exists = []
        for component in components:
            try:
                pkg = ("%s:%s" % (namespace, component))
                ret = self.bintray.get_version(subject, repo, pkg, tag)
                if ret['error'] is True:
                    exists.append(component)
            except Exception as e:
                self.logger.exception("Image version not found %s %s" %
                                    (pkg, e))
                exists.append(component)
                continue
        return exists

    def build_image_from_branch(self, component, rootdir, dockerfile,
                                tag="latest"):
        """
            Build docker image from path
            component: Component to build
            rootdir: Directory to source Dockerfile
            dockerfile: Dockerfile name
        """
        try:
            self.logger.info("Building image with %s from %s" %
                             (dockerfile, rootdir))
            image, _ = self.docker_client.images.build(path=rootdir,
                                            dockerfile=dockerfile,
                                            tag="%s:%s" % (component, tag))
            self.logger.info("Built image %s with tag %s" % (component, tag))
            return image.short_id
        except Exception as e:
            self.logger.exception("Error occurred when trying to build "
                                 "image %s" % e)
            return None

    def dump_changelog_from_image(self, latest, previous,
                                    component, outfile=None):
        """
            Get commit shas from image properties
            given two bintray version tags for a repo and dump changelog
            between the two commits
        """
        h_commit = self.get_bintray_image_property(cdn.BINTRAY_COMMIT_LABEL,
                                    component, version=latest)
        l_commit = self.get_bintray_image_property(cdn.BINTRAY_COMMIT_LABEL,
                                    component, version=previous)
        self.logger.info("Latest version commit %s, Current version commit %s" %
                            h_commit[0], l_commit[0])
        changelog = utils.get_changelog(h_commit[0], l_commit[0],
                                        cdn.GITLAB_DEFAULT_REPO)
        if outfile is not None:
            with open(outfile, "w") as filehandle:
                filehandle.write("\n".join(changelog))
            self.logger.info("Successfully dumped changelog to file" % outfile)
        else:
            self.logger.info("\n".join(changelog))
