/**
 * Manage the shared Jenkins library needed for source code builds.
**/


/**
 * Attempt to load a branch of the PipelineWorks shared Jenkins library.
 *
 * @param branch The branch of the library repository that should be loaded
**/
void loadLibrary(String branch) {
    // libName matches one of the entries in Jenkins > Manage Jenkins >
    // Configure System > Global Pipeline Libraries.
    // That is where Jenkins will get libraries such as gitlabBuildSteps.
    // The library() call makes anything in the library available.
    // We use the library() call instead of the @Library directive
    // because we want to choose what to load, and @Library requires
    // that it be hard coded.
    
    String libName = 'PipelineWorks'
    library("${libName}@${branch}").org.vmware
}

def runBuild() {
    if (params.shared_lib_branch?.trim()) {
        // The caller has requested a specific branch by specifying the
        // shared_lib_branch parameter
        echo("Loading PipelineWorks. Explicitly given ${params.shared_lib_branch}.")
        loadLibrary(params.shared_lib_branch)
    }
    else {
        // The straightforward case is to ude master for PipelineWorks.
        echo 'Loading PipelineWorks. Using the master branch.'
        loadLibrary('master')
    }

    cleanupSddcResources()
}

return this
