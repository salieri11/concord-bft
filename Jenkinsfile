node {
    // We use the "library" step here instead of the @Library directive at the
    // top of the file because we want to load the Builder class from whatever
    // the current branch is, and @Library requires that it be hard coded.
    def builder
    def buildLib

    if (params.shared_lib_branch && params.shared_lib_branch.trim()) {
      echo "Shared lib load: Explicitly given ${params.shared_lib_branch}."
      buildLib = library("BlockchainSharedJenkinsLib@${params.shared_lib_branch}").org.vmware
    } else if (env.BRANCH_NAME && env.BRANCH_NAME.trim()) {
      echo "Shared lib load: Using branch ${env.BRANCH_NAME}."
      buildLib = library("BlockchainSharedJenkinsLib@${env.BRANCH_NAME}").org.vmware
    } else {
      echo "Shared lib load: Using master."
      buildLib = library("BlockchainSharedJenkinsLib@master").org.vmware
    }

    builder = buildLib.Builder.new(this)
    builder.startBuild()
}
