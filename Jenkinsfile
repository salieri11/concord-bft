node {
    // We use the "library" step here instead of the @Library directive at the
    // top of the file because we want to load the Builder class from whatever
    // the current branch is, and @Library requires that it be hard coded.
    def builder
    def buildLib
    def libName

    scmUrl = scm.userRemoteConfigs[0].url

    // libName matches one of the entries in Jenkins > Manage Jenkins >
    // Configure System > Global Pipeline Libraries.
    libName = "JenkinsLibOnGitlab"

    if (params.shared_lib_branch && params.shared_lib_branch.trim()) {
      echo "Shared lib load: Explicitly given ${params.shared_lib_branch}."
      buildLib = library(libName + "@${params.shared_lib_branch}").org.vmware
    } else if (env.gitlabSourceBranch && env.gitlabSourceBranch.trim()) {
      echo "Shared lib load: Using branch ${env.gitlabSourceBranch}."
      buildLib = library(libName + "@${env.gitlabSourceBranch}").org.vmware
      echo "Shared lib load: Using master."
      buildLib = library(libName + "@master").org.vmware
    }

    builder = buildLib.Builder.new(this, usingGitlab)
    builder.startBuild()
}
