package org.vmware

class Builder {
  def steps
  def usingGitlab

  // Enables using Jenkins steps in a shared library.  e.g. steps.echo ...
  Builder(steps, usingGitlab) {
     this.steps = steps
     this.usingGitlab = usingGitlab
  }

  // Main entry point for the entire process
  def startBuild(){
    if(usingGitlab){
      steps.gitlabBuildSteps()
    }else{
      // Github, going away.
      steps.mainBuildSteps()
    }
  }
}
