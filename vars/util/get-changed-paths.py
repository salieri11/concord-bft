#!/usr/bin/python3

#########################################################################
# Copyright 2018 - 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import json
import subprocess
import traceback
import argparse
import os

def main():
  try:

    # First get which ToT branch to compared to
    parser = argparse.ArgumentParser()
    parser.add_argument("--totConfig",
                        help="File path of build_info.json",
                        default="build_info.json")
    cmdlineArgs = parser.parse_args()

    DIFF_BRANCH = "master"
    if os.path.exists(cmdlineArgs.totConfig):
      with open(cmdlineArgs.totConfig, 'r') as f:
        DIFF_BRANCH = json.load(f)["tot_branch"]
    else:
      print("WARNING: target ToT branch cannot be parsed; using 'master'")

    print("Comparing against branch '{}' to detect file changes in this MR.".format(DIFF_BRANCH))
    # Get utf8 string output of `git diff` of this MR compared to origin/master
    # --name-only gets only the affected file paths
    # --exit-code exits the command without hanging in the editor view
    diffOutput = subprocess.run([
        'git', 'diff', 'origin/' + DIFF_BRANCH, '--name-only', '--exit-code'
        ], stdout=subprocess.PIPE).stdout.decode('utf-8')

    diffOutputLines = diffOutput.split('\n')
    # ! Edge case: Between this MR's local ToT merge and above diff command (~30 second gap),
    # origin/master can be changed (e.g. someone clicks 'merge' on another MR which is already approved)
    # This is very highly unlikely, but if so, this diff command will likely list MORE files than
    # local changes in this MR. The only foul-proof diff can be extracted from the actual `git merge` output.
    # Listing more file changes is not a problem, since it will NEVER decrease the testing scope of the run
    # (unless, this MR does EXACTLY opposite file change of the intruded MR, which is extremely unlikely).
    # In almost all thinkable race conditions, Jenkins will build MORE and test MORE, not LESS.

    paths = []
    for line in diffOutputLines:
      changedFilePath = line.strip()
      # Ignore null paths
      if changedFilePath is None or len(changedFilePath) == 0:
        continue
      # WARNING: ui version.json always gets changed during "Get Version" stage in groovy; ignore it.
      if changedFilePath == "ui/src/static/data/version.json":
        continue
      # README's and markdown files are agreed upon to have no functional impact
      if changedFilePath.endswith("README"):
        print(f"{changedFilePath} excluded => README non-functional")
        continue
      if changedFilePath.lower().endswith(".md"):
        print(f"{changedFilePath} excluded => *.md non-functional")
        continue
      # Config files for git and gitlab should have no functional impact
      if changedFilePath == "CODEOWNERS":
        print(f"{changedFilePath} excluded => CODEOWNERS non-functional")
        continue
      paths.append(changedFilePath)

    # Bring map of known components/folders
    componentsMap = {}
    with open("components_affected.json", "r") as f:
      componentsMap = json.loads(f.read())

    # subComponents, e.g. concord/src/daml or concord/src/ethereum
    # e.g. concord changed, but only ethereum sub-component changed ===> no need to do full vDAML test suite
    subComponents = list(filter(lambda path: (path.find("/") >= 0), componentsMap.keys()))

    # Default committed `components_affected.json` has all set to TRUE (build/test everything)
    # This is to prevent unwarranted skipping of builds/tests when:
    # 1) This script `getChangedPath.py` itself never triggers for some reason (or intentionally),
    # 2) Possible errors with file open/write; relative path, bad file permissions, etc.
    # If file read is successful, ZERO OUT the map to False
    for componentName in componentsMap:
      componentsMap[componentName]["changed"] = False

    # Iterate through changes and set to True where functional changes are detected
    for path in paths:
      folderName = path.split('/')[0] if path.find('/') >= 0 else 'root'
      if folderName is not None:
        if folderName in componentsMap:
          # known folder/component, changed = true
          componentsMap[folderName]["changed"] = True
          print(f"{folderName} changed ({path})")
        else:
          componentsMap["unknown"]["changed"] = True # unknown directory change => build/test all
          print(f"{folderName} changed ({path}) => UNCAUGHT directory change")
      for subComponent in subComponents:
        if path.startswith(subComponent):
          componentsMap[subComponent]["changed"] = True
          print(f"{subComponent} changed ({path}) => sub-component change")

    if len(paths) == 0: # no functional changes in this MR (e.g. the changes are only *.md, README, etc.)
      componentsMap["nothing"]["changed"] = True # No need to bulid or test; just pass the pipeline right away

    # Uncaught/unregistered path change will take precaution and cover everything
    if componentsMap["unknown"]["changed"]:
      print("Uncaught path change is detected; this run will build and test EVERYTHING.")
      componentsMap["nothing"]["changed"] = False

    print("================================================================================")
    print("Affected components in this MR: ")
    print(json.dumps(componentsMap, indent = 4))
    print("================================================================================")

    # Flush the changed components map to file
    with open("components_affected.json", "w") as f:
      f.write(json.dumps(componentsMap, indent = 4))

  except Exception as e:
    # Any failure while detecting should take high caution; EVERYTHING must be built and tested.
    traceback.print_exc()
    print("Something went wrong while detecting component changes; this run will build and test EVERYTHING.")



if __name__ == "__main__":
  main()
