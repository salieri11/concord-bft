package org.vmware

class Builder {
  def steps

  // Enables using Jenkins steps in a shared library.  e.g. steps.echo ...
  Builder(steps) {
     this.steps = steps
  }

  // Returns all repositories with the branch name passed in.
  // Not sure if we'll use it, but it's good to know we can if needed.
  def getReposWithThisBranch(String branchToFind) {
    def reposToCheck = ["git@github.com:vmwathena/athena.git",
                        "git@github.com:vmwathena/helen.git",
                        "git@github.com:vmwathena/hermes.git"]
    def reposWithBranch = []

    for (repoToCheck in reposToCheck){
      // Note that this git command required putting the ssh keys in
      // /var/lib/jenkins/.ssh and setting their ownership to "jenkins".
      def stdOut = steps.sh (script: "git ls-remote ${repoToCheck.value} ${branchToFind}", returnStdout: true)

      if (stdOut?.trim()) {
        def start = repoToCheck.lastIndexOf("/") + 1
        def end = repoToCheck.lastIndexOf(".") - 1
        reposWithBranch.add(new String(repoToCheck[start..end]))
      }
    }

    return reposWithBranch
  }

  // Main entry point for the entire process
  def startBuild(){
    steps.echo("Running.  Branch is ${steps.env.BRANCH_NAME}")
    steps.mainBuildSteps()
  }
}
