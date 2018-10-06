node {
    // We use the "library" step here instead of the @Library directive at the
    // top of the file because we want to load the Builder class from whatever
    // the current branch is, and @Library requires that it be hard coded.
    def sharedLibBranch

    if (params.shared_lib_branch_or_commit && params.shared_lib_branch_or_commit.trim()) {
       sharedLibBranch = params.shared_lib_branch_or_commit
    } else if (env.BRANCH_NAME && env.BRANCH_NAME.trim()) {
      sharedLibBranch = BRANCH_NAME
    } else {
      sharedLibBranch = "master"
    }

    buildLib = library("SharedJenkinsLib@${sharedLibBranch}").org.vmware
    builder = buildLib.Builder.new(this)
    builder.startBuild()
}
