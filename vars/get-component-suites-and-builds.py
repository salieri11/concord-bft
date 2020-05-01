#!/usr/bin/python3

#################################################################################
# Copyright 2018 - 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# Each source subdirectory in components_affected.json may contain a file with
# the following format:
#
#   {
#     "test_suites": [],
#     "build_targets": []
#   }
#
# This utility loops through all of the directories in components_affected.json,
# looks for the above file in each, and if found, adds its contents to the
# main file.
#################################################################################
import json
import os
import tempfile


MAIN_MAP_FILE = "components_affected.json"
COMPONENT_FILE = "tests_and_artifacts.json"
SUITES_KEY = "test_suites"
BUILDS_KEY = "build_targets"
IGNORE_KEYS = ["unknown", "nothing"]

def main():
  src_dir_map = get_map()
  src_dir_map = get_suites_and_builds(src_dir_map)
  write_map(src_dir_map)


def get_map():
  '''
  Load and return the main file (in vars) which will later be read by
  gitlabBuildSteps.groovy.
  '''
  if os.path.isfile(MAIN_MAP_FILE):
    with open(MAIN_MAP_FILE, "r") as f:
      return json.loads(f.read())
  else:
    raise Exception("Error: '{}' not found.".format(MAIN_MAP_FILE))


def write_map(src_dir_map):
  '''
  Atomically write the file with the new data.
  '''
  with tempfile.NamedTemporaryFile("w", delete=False) as f_out:
    f_out.write(json.dumps(src_dir_map, indent = 4))
    f_out.flush()
    os.fsync(f_out.fileno())

  # Make read/write for other users so subsequent Hermes runs which
  # do not use sudo (e.g. UI tests) won't fail.
  os.replace(f_out.name, MAIN_MAP_FILE)
  os.chmod(MAIN_MAP_FILE, 0o664)


def get_suites_and_builds(src_dir_map):
  '''
  '''
  for src_key in src_dir_map:
    if src_key == "root":
      src_dir = ".."
    elif src_key in IGNORE_KEYS:
      continue
    else:
      src_dir = os.path.join("..", src_key)

    if not os.path.isdir(src_dir):
      raise Exception("Error: Directory '{}' (defined in '{}') does not exist.".format(src_dir, MAIN_MAP_FILE))

    component_file_path = os.path.join(src_dir, COMPONENT_FILE)

    if os.path.exists(component_file_path):
      with open(component_file_path, "r") as f:
        component_suites_and_builds = json.loads(f.read())

      if SUITES_KEY in component_suites_and_builds and \
         component_suites_and_builds[SUITES_KEY]:
        print("Using suites {} for {}".format(component_suites_and_builds[SUITES_KEY], src_key))
        src_dir_map[src_key][SUITES_KEY] = component_suites_and_builds[SUITES_KEY]

      if BUILDS_KEY in component_suites_and_builds and \
         component_suites_and_builds[BUILDS_KEY]:
        print("Building {} for {}".format(component_suites_and_builds[BUILDS_KEY], src_key))
        src_dir_map[src_key][BUILDS_KEY] = component_suites_and_builds[BUILDS_KEY]

    else:
      print("'{}' not found in '{}' so defaults will be used.".format(
        COMPONENT_FILE, src_dir))

  return src_dir_map


if __name__ == "__main__":
  main()
