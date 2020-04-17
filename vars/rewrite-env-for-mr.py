#!/usr/bin/python3

#################################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# The file components_affected.json contains items with the following structure:
#
# "concord": {
#     "changed": true,
#     "test_suites": [],
#     "build_targets": []
# },
#
# This utility loops through these structures to find what needs to be built,
# and constructs a .env file which contains docker images to build and docker
# images which can be pulled from artifactory.
#################################################################################
import argparse
import json
import os
import re
import subprocess
import tempfile


MAIN_MAP_FILE = "components_affected.json"
CHANGED_KEY = "changed"
BUILDS_KEY = "build_targets"
IGNORE_KEYS = ["unknown", "nothing"]
BUILD_NOTHING_INDICATOR = "none"

def main():
  parser = argparse.ArgumentParser()
  parser.add_argument("--artifactoryKey",
                      help="Your artifactory API key, if not in the ARTIFACTORY_KEY " \
                      "environment variable.",
                      default=None)
  args = parser.parse_args()
  new_env = get_new_env(args.artifactoryKey)
  images_to_build = get_images_to_build(new_env)
  replace_env(new_env, images_to_build)


def get_new_env(artifactory_key):
  '''
  Call make-prebuilt-env and return the output.
  Raises CalledProcessError if nonzero exit code.
  '''
  if not "ARTIFACTORY_KEY" in os.environ:
    os.environ["ARTIFACTORY_KEY"] = artifactory_key

  cmd = ["../docker/make-prebuilt-env.sh"]
  proc = subprocess.run(cmd, capture_output=True, cwd="../docker")
  if proc.returncode != 0:
    print("Calling make-prebuilt-env failed.")
    if proc.stdout and proc.stdout.decode("UTF-8").strip():
      print("stdout: '{}'".format(proc.stdout.decode("UTF-8").strip()))
    if proc.stderr and proc.stderr.decode("UTF-8").strip():
      print("stderr: '{}'".format(proc.stderr.decode("UTF-8").strip()))

  proc.check_returncode()
  return proc.stdout.decode("UTF-8")


def get_images_to_build(new_env):
  '''
  Look through values in the file of directories and things to build.
  If a directory changed, add its artifacts to a list which is returned.
  '''
  images_to_build = []

  with open(MAIN_MAP_FILE, "r") as f:
    component_map = json.loads(f.read())

  for component in component_map:
    if CHANGED_KEY in component_map[component] and \
       component_map[component][CHANGED_KEY]:

      if BUILDS_KEY in component_map[component] and \
         component_map[component][BUILDS_KEY]:
          if component_map[component][BUILDS_KEY] == BUILD_NOTHING_INDICATOR:
            print("Explicitly told not to build anything for '{}' even though " \
                  "it changed.".format(component))
          else:
            images_to_build.extend(component_map[component][BUILDS_KEY])
      else:
        # The directory changed, and we were not told to build or to not build
        # anything, so build everything.
        print("Directory '{}' changed, but there is nothing specified " \
              "about what to build. Building everything.".format(component))
        images_to_build = get_all_images(new_env)

  return images_to_build


def get_all_images(env_lines):
  '''
  Return all images from the env file.  These are just the short names
  like "concord-core", without the full path to the repo.
  '''
  images = []

  for line in env_lines.split("\n"):
    if line.strip():
      key, val = line.split("=")

      if key.endswith("_repo"):
        val = re.sub("/[a-f0-9]+\.[a-f0-9]+\.[a-f0-9]+", "", val)
        images.append(val.split("/")[-1])

  return images


def replace_env(new_env, images_to_build):
  '''
  Accepts the output of make-prebuilt-env (.env file format) and a list
  of images to build. For every image to build, replaces the entry in .env
  with a format indicating that it has to be built instead of pulled from
  artifactory. Everything not replaced here will get pulled from artifactory
  at build time.
  '''
  with open("../docker/.env", "r") as f:
    old_env = f.read()

  old_env_lines = old_env.split("\n")
  new_env_lines = new_env.split("\n")

  # buildall.sh is not expecting it to end in the version yet.
  for i, line in enumerate(new_env_lines):
    new_env_lines[i] = re.sub("/[a-f0-9]+\.[a-f0-9]+\.[a-f0-9]+", "", new_env_lines[i])

  for image_to_build in images_to_build:
    for i, new_env_repo_line in enumerate(new_env_lines):
      if new_env_repo_line:
        repo_key, repo_val = new_env_repo_line.split("=")  # e.g. concord_repo=athena-docker-local.artifactory.eng.vmware.com/concord-core
        repo_component = repo_val.split("/")[-1]

        if image_to_build == repo_component:
          # Now we have this from the new env lines:
          #   concord_repo=athena-docker-local.artifactory.eng.vmware.com/concord-core
          #
          # We know we have to build this, so set concord_repo to the original tag.
          old_tag = get_matching_tag_for_repo(repo_key, old_env_lines)
          set_tag_for_repo(repo_key, old_tag, new_env_lines)

  # Now add non-repository lines from old_env, like LINT_API_KEY, etc...
  new_env_keys = []
  for line in new_env_lines:
    if line and line.strip():
      new_env_keys.append(line.split("=")[0])

  for line in old_env_lines:
    if line and line.strip():
      old_env_key = line.split("=")[0]

      if not old_env_key in new_env_keys:
        new_env_lines.append(line.strip())

  with tempfile.NamedTemporaryFile("w", delete=False) as f_out:
    f_out.write("\n".join(new_env_lines))
    f_out.flush()
    os.fsync(f_out.fileno())

  os.replace(f_out.name, "../docker/.env")
  os.chmod("../docker/.env", 0o664)


def get_matching_tag_for_repo(repo_key, env_lines):
  '''
  Given a key like "concord_repo", return the "concord_tag" value
  from the given env_lines.
  '''
  prefix = repo_key.rsplit("_", 1)[0]
  find_key = prefix + "_tag"

  for line in env_lines:
    if line and line.strip():
      k, v = line.split("=")
      if k == find_key:
        return v


def set_tag_for_repo(repo_key, tag, env_lines):
  '''
  Given a key like "concord_repo", find "concord_tag" in env_lines
  and set it to the given tag.
  '''
  prefix = repo_key.rsplit("_", 1)[0]
  find_key = prefix + "_tag"

  for i, line in enumerate(env_lines):
    if line and line.strip():
      k, v = line.split("=")

      if k == find_key:
        env_lines[i] = find_key + "=" + tag



if __name__ == "__main__":
  main()
