package org.vmware

class Builder {
  def steps

  // Enables using Jenkins steps in a shared library.  e.g. steps.echo ...
  Builder(steps) {
     this.steps = steps
  }

  // Main entry point for the entire process
  def startBuild(){
    steps.mainBuildSteps()
  }
}
