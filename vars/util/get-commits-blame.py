#!/usr/bin/python3

#########################################################################
# Copyright 2018 - 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import json
import subprocess
import traceback
import argparse
import os

SPECIAL_FLAGS_FILE = None 
if os.getenv("WORKSPACE"):
  SPECIAL_FLAGS_FILE = os.getenv("WORKSPACE") + '/summary/special_flags.json'

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

    print("Detecting commits and who changed things in this MR...")
    # Get utf8 string output of `git log` of current local branch
    # against origin/master to fetch commits and authors (recent commits first)
    # e.g. > git log origin/master.. --pretty=format:"%H%x09%aN%x09%aE%x09%aD%x09%s"
    # 4bda290d71e0391b8cc862134aff68895ef31b94   John Doe   user@vmware.com   Tue, 10 Mar 2020 22:42:21 -0700   commit 2
    # 7817802fc3f1ec7aabcf306c4d7ee1e37f2f64f7   John Doe   user@vmware.com   Tue, 10 Mar 2020 23:22:27 -0700   commit 1

    commitsOutput = subprocess.run([
        'git', 'log', 'origin/' + DIFF_BRANCH + '..',
        '--pretty=format:"%H%x09%aN%x09%aE%x09%aD%x09%s%x09%b"'
        # More info: https://mirrors.edge.kernel.org/pub/software/scm/git/docs/git-log.html#_pretty_formats
        # %H = Full Commit Hash
        # %x09 = Tab Character (char code 9)
        # %aN = Author Name (respecting .mailmap)
        # %x09 = Tab Character (char code 9)
        # %aE = Author Email (respecting .mailmap)
        # %x09 = Tab Character (char code 9)
        # %aD = Author Date (RFC2822 style)
        # %x09 = Tab Character (char code 9)
        # %s = Commit Summary
        # %x09 = Tab Character (char code 9)
        # %s = Commit Message Body
        ], stdout=subprocess.PIPE).stdout.decode('utf-8')
    commitsOutput = commitsOutput.split('\n')
    
    commitsAuthors = {
      "processed": True,
      "authorsList": "", # comma separated list of emails
      "commits": [],
      "blame": []
    }

    allSpecialFlags = {}

    authorsMap = {}

    # preprocess commits with new lines
    currentCommit = ''
    preprocessedCommits = []
    for line in commitsOutput:
      tabCount = len(line.split('\t'))
      if tabCount < 6:
        currentCommit += '\n' + line
      else:
        if currentCommit:
          preprocessedCommits.append(currentCommit)
        currentCommit = line
    if currentCommit:
      preprocessedCommits.append(currentCommit)

    for commit in preprocessedCommits:
      commitInfoStr = commit.strip()
      # Ignore null line
      if commitInfoStr is None or len(commitInfoStr) == 0:
        continue
      # strip unwanted quotes
      while commitInfoStr.startswith('"') and commitInfoStr.endswith('"'):
        commitInfoStr = commitInfoStr[1:-1]
      commitInfo = commitInfoStr.split('\t')
      if len(commitInfo) != 6:
        print(f"git log is not displaying results as expected, commit: {commitInfoStr}")
        continue
      authorEmail = commitInfo[2]
      summary = commitInfo[4]
      body = commitInfo[5]
      if summary.startswith("Merge branch ") or authorEmail == "vmwathenabot@vmware.com":
        print(f"Merging commit skipped, '{summary}'")
        continue
      # scan for special flags
      specialFlags = detectSpecialFlags(summary, body)
      for key in specialFlags:
        allSpecialFlags[key] = specialFlags[key]
      if authorEmail not in authorsMap:
        commitsAuthors["blame"].append(authorEmail)
        authorsMap[authorEmail] = True # prevent dupe
      commitInfo = {
        "id": commitInfo[0],
        "author": commitInfo[1],
        "email": commitInfo[2],
        "time": commitInfo[3],
        "summary": commitInfo[4],
        "body": commitInfo[5],
      }
      if len(specialFlags.keys()) > 0:
        commitInfo["special_flags"] = specialFlags
      commitsAuthors["commits"].append(commitInfo)
    
    # also add comma-separated list for easy handling in command line
    blame = ",".join(commitsAuthors["blame"])
    commitsAuthors["authorsList"] = blame

    print("================================================================================")
    print("Effective commits in this MR: ")
    print(json.dumps(commitsAuthors["commits"], indent=4))
    print(f"Blame: {blame}")
    print("================================================================================")

    # Flush the processed result
    with open("commits_authors.json", "w") as f:
      f.write(json.dumps(commitsAuthors, indent=4))

    if len(allSpecialFlags.keys()) > 0:
      print("")
      print("!!!  SPECIAL FLAGS DETECTED from commits.")
      print("     This might change the run behavior.")
      print("")
      print(json.dumps(allSpecialFlags, indent=4))
      print("================================================================================")

    # Flush special commit flags; starts with #!
    if SPECIAL_FLAGS_FILE and len(allSpecialFlags.keys()) > 0:
      with open(SPECIAL_FLAGS_FILE, "w") as f:
        f.write(json.dumps(allSpecialFlags, indent=4))

  except Exception as e:
    traceback.print_exc()
    print("Something went wrong while detecting commits and authors list")


def detectSpecialFlags(summary, body):
  specialFlags = {}
  try:
    words = (summary + '\n' + body).replace(';', ' ')\
            .replace('\n', ' ').split(' ')
    for word in words:
      word = word.strip()
      if word.startswith('#!'):
        word = word[2:] # drop #!
        if '=' in word:
          key = word.split('=')[0]
          value = word.split('=')[1]
          specialFlags[key] = value
        else:
          specialFlags[word] = True
  except Exception as e:
    traceback.print_exc()
    print(f"Special flag parsing failed for: {summary}")
  return specialFlags


if __name__ == "__main__":
  main()
