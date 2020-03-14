def runBuild(){
  // libName matches one of the entries in Jenkins > Manage Jenkins >
  // Configure System > Global Pipeline Libraries.
  // That is where Jenkins will get libraries such as gitlabBuildSteps.
  // The library() call makes anything in the library available.
  // We use the library() call instead of the @Library directive
  // because we want to choose what to load, and @Library requires
  // that it be hard coded.
  libName = "JenkinsLibOnGitlab"

  if (params.shared_lib_branch && params.shared_lib_branch.trim()) {
    echo "Shared lib load: Explicitly given ${params.shared_lib_branch}."
    library(libName + "@${params.shared_lib_branch}").org.vmware
  } else if (env.gitlabSourceBranch && env.gitlabSourceBranch.trim()) {
    echo "Shared lib load: Using branch ${env.gitlabSourceBranch}."
    library(libName + "@${env.gitlabSourceBranch}").org.vmware
  } else {
    echo "Shared lib load: Using master."
    library(libName + "@master").org.vmware
  }

  gitlabBuildSteps()
}

return this
