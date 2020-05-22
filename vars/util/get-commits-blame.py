#!/usr/bin/python3

#########################################################################
# Copyright 2018 - 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################

import json
import subprocess
import traceback

def main():
  try:
    print("Detecting commits and who changed things in this MR...")
    # Get utf8 string output of `git log` of current local branch
    # against origin/master to fetch commits and authors (recent commits first)
    # e.g. > git log origin/master.. --pretty=format:"%H%x09%aN%x09%aE%x09%aD%x09%s"
    # 4bda290d71e0391b8cc862134aff68895ef31b94   John Doe   user@vmware.com   Tue, 10 Mar 2020 22:42:21 -0700   commit 2
    # 7817802fc3f1ec7aabcf306c4d7ee1e37f2f64f7   John Doe   user@vmware.com   Tue, 10 Mar 2020 23:22:27 -0700   commit 1
    commitsOutput = subprocess.run([
        'git', 'log', 'origin/master..',
        '--pretty=format:"%H%x09%aN%x09%aE%x09%aD%x09%s"'
        # More info: https://mirrors.edge.kernel.org/pub/software/scm/git/docs/git-log.html
        # %H = Full Commit Hash
        # %x09 = Tab Character (char code 9)
        # %aN = Author Name (respecting .mailmap)
        # %x09 = Tab Character (char code 9)
        # %aE = Author Email (respecting .mailmap)
        # %x09 = Tab Character (char code 9)
        # %aD = Author Date (RFC2822 style)
        # %x09 = Tab Character (char code 9)
        # %s = Commit Summary
        ], stdout=subprocess.PIPE).stdout.decode('utf-8')
    commitsOutput = commitsOutput.split('\n')
    
    commitsAuthors = {
      "processed": True,
      "authorsList": "", # comma separated list of emails
      "commits": [],
      "blame": []
    }

    authorsMap = {}
    for line in commitsOutput:
      commitInfoStr = line.strip()
      # Ignore null line
      if commitInfoStr is None or len(commitInfoStr) == 0:
        continue
      # strip unwanted quotes
      while commitInfoStr.startswith('"') and commitInfoStr.endswith('"'):
        commitInfoStr = commitInfoStr[1:-1]
      commitInfo = commitInfoStr.split('\t')
      if len(commitInfo) != 5:
        print(f"git log is not displaying results as expected, line: {commitInfoStr}")
        continue
      authorEmail = commitInfo[2]
      summary = commitInfo[4]
      if summary.startswith("Merge branch ") or authorEmail == "vmwathenabot@vmware.com":
        print(f"Merging commit skipped, '{summary}'")
        continue
      if authorEmail not in authorsMap:
        commitsAuthors["blame"].append(authorEmail)
        authorsMap[authorEmail] = True # prevent dupe
      commitsAuthors["commits"].append({
        "id": commitInfo[0],
        "author": commitInfo[1],
        "email": commitInfo[2],
        "time": commitInfo[3],
        "summary": commitInfo[4],
      })
    
    # also add comma-separated list for easy handling in command line
    blame = ",".join(commitsAuthors["blame"])
    commitsAuthors["authorsList"] = blame

    print("================================================================================")
    print("Effective commits in this MR: ")
    print(json.dumps(commitsAuthors["commits"], indent = 4))
    print(f"Blame: {blame}")
    print("================================================================================")

    # Flush the processed result
    with open("vars/util/commits_authors.json", "w") as f:
      f.write(json.dumps(commitsAuthors, indent = 4))
  
  except Exception as e:
    traceback.print_exc()
    print("Something went wrong while detecting commits and authors list")



if __name__ == "__main__":
  main()