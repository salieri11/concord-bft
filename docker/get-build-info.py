import argparse
import json
import os
import requests

ARTIFACTORY_BASE_URL = "https://build-artifactory.eng.vmware.com/api/storage/athena-docker-local/"
ARTIFACTORY_MANIFEST_URL = ARTIFACTORY_BASE_URL + "{}/{}/manifest.json?properties"
ARTIFACTORY_LASTMOD_URL = ARTIFACTORY_BASE_URL + "{}?lastModified"
KEY_HEADER = "X-JFrog-Art-Api"
LATEST_TAG_FILE_PATH = os.getenv("WORKSPACE") + '/blockchain/vars/latest_tag.json' if os.getenv("WORKSPACE") else None

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--build",
                        help="The build to look up.  Defaults to 'latest' and looks " \
                        "up the latest.",
                        default="latest")
    parser.add_argument("--artifactoryKey",
                        help="Your artifactory API key.  May also be set as " \
                        "the environment variable ARTIFACTORY_KEY.  The command " \
                        "line one will be used if specifying both.",
                        default=None)
    parser.add_argument("--component",
                        help="The component to look up. Typically, all components " \
                        "are uploaded together, so this should not matter, but " \
                        "you never know. Defaults to 'agent'.",
                        default="agent")
    cmdlineArgs = parser.parse_args()
    perform_precheck(cmdlineArgs)
    buildData = retrieve_build_data(cmdlineArgs)
    display_data(cmdlineArgs, buildData)


def perform_precheck(cmdlineArgs):
    if not cmdlineArgs.artifactoryKey and \
       not "ARTIFACTORY_KEY" in os.environ:
        raise Exception("Set --artifactoryKey on the commnad line or set the " \
                        "ARTIFACTORY_KEY environment variable.")


def retrieve_build_data(cmdlineArgs):
    api_key = get_artifactory_key(cmdlineArgs)
    build = get_build(cmdlineArgs, api_key)
    response = requests.get(ARTIFACTORY_MANIFEST_URL.format(cmdlineArgs.component, build),
                        headers={KEY_HEADER: api_key})
    check_response(cmdlineArgs, response)
    return json.loads(response.content.decode("UTF-8"))


def get_build(cmdlineArgs, api_key):
    if cmdlineArgs.build == "latest":
        response = requests.get(ARTIFACTORY_LASTMOD_URL.format(cmdlineArgs.component),
                                headers={KEY_HEADER: api_key})
        check_response(cmdlineArgs, response)
        content = json.loads(response.content.decode("UTF-8"))
        uri = content["uri"]
        s = uri.split(cmdlineArgs.component + "/")[1]
        buildNumber = s.split("/")[0]
        if LATEST_TAG_FILE_PATH:
          with open(LATEST_TAG_FILE_PATH, "w+") as f:
            f.write(json.dumps({"latest_tag":buildNumber}, indent = 4))
        return buildNumber
    else:
        return cmdlineArgs.build
        
        
def check_response(cmdlineArgs, response):
    if response.status_code != 200 or \
       not response.content:

        err_msg = "Received invalid response: {}".format(response)
        err_msg += "\nfor component: {}, build: {}".format(cmdlineArgs.component,
                                                       cmdlineArgs.build)
        if response.content:
            err_msg += "\n" + response.content.decode("UTF-8")

        raise Exception(err_msg)


def get_artifactory_key(cmdlineArgs):
    return cmdlineArgs.artifactoryKey if cmdlineArgs.artifactoryKey else \
        os.environ["ARTIFACTORY_KEY"]


def display_data(cmdlineArgs, buildData):
    labels = {}
    label_marker = "docker.label."
    
    for prop in buildData["properties"]:
        if prop.startswith(label_marker):
            label_key = prop[len(label_marker):]
            label_val = buildData["properties"][prop][0]
            labels[label_key] = label_val

    print("Labels for build {}, component {}:".format(cmdlineArgs.build,
                                                     cmdlineArgs.component))
    for label in sorted(labels.keys()):
        print("   {}: {}".format(label, labels[label]))

main()
