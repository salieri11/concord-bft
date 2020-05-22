// All that varies for each repo is the branch, so wrap this very large call.
void checkoutRepo(repo_url, branch_or_commit){
    maxAttempts = 10
    attempts = 0
    sleepTime = 10
    success = false

    while (attempts < maxAttempts && !success){
        try{
            checkout([$class: 'GitSCM', branches: [[name: branch_or_commit]], doGenerateSubmoduleConfigurations: false, extensions: [[$class: 'SubmoduleOption', disableSubmodules: false, parentCredentials: true, recursiveSubmodules: true, reference: '', trackingSubmodules: false]], submoduleCfg: [], userRemoteConfigs: [[credentialsId: 'GITLAB_LDAP_CREDENTIALS', url: repo_url]]])
            success = true
        }catch(Exception e){
            echo "Checkout failed"

            if (attempts < maxAttempts){
                echo "Retrying in " + sleepTime + " seconds."
                attempts += 1
                sleep(sleepTime)
            }
        }
    }

    if (!success){
        error("Reached maximum number of attempts to check out the repo. Failing.")
    }
}

// Creates a git tag and commits it. Must be called when the pwd is the
// source git directory.
void createAndPushGitTag(tag){
    sh (
            script: "git tag -a ${tag} -m 'Version tag created by the build system'",
            returnStdout: false
    )

    sh (
            script: "git push origin ${tag}",
            returnStdout: false
    )
}

// Returns all changes since the last git tag.
String getChangesSinceLastTag(){
    return sh (
            script: 'git log `git tag -l --sort=-v:refname | head -n 1`..HEAD',
            returnStdout: true
    ).trim()
}

// Returns whether this is a GitLab run.
Boolean isGitLabRun(){
    return env.gitlabSourceBranch && env.gitlabSourceBranch.trim()
}

// We are going to build/test against what things will look like after merge.
// These steps to merge were taken from GitLab, so we should be getting exactly
// what GitLab will get when it does a merge, unless someone else merges first.
void mergeToTot(branch_or_commit, merge_output_log){
    if (branch_or_commit){
        sh(script: "git checkout ${branch_or_commit}")
        branchHash = getHead()

        sh(script: "git checkout ${tot_branch}")
        origTotHash = getHead()

//        sh(script: "git merge --no-ff ${branch_or_commit} | tee ${merge_output_log}")
        sh(script: "git merge --no-ff ${branch_or_commit}")

        sh(script: "git submodule update --recursive")
        newTotHash = getHead()

        echo "Merged '" + branch_or_commit + "' " +
             "(hash '" + branchHash + "') with ToT at commit " +
             "'" + origTotHash + "' resulting in new " +
             "ToT hash '" + newTotHash + "'.  This is just a " +
             "local merge and will not be pushed."
    }
}

String getHead(){
    return sh (
            script: "git rev-parse --short HEAD",
            returnStdout: true
    ).trim()
}

// Called when it begins.
// Don't call for individual stages.
void startRun(){
    updateGitlabCommitStatus(name: "Jenkins Run", state: "running")
}

// Called when it fails.
void failRun(Exception ex = null){
    updateGitlabCommitStatus(name: "Jenkins Run", state: "failed")
    if (ex != null){
        echo "The run has failed with an exception: " + ex.toString()
    }
}

// The user's parameter is top priority, and if it fails, let an exception be thrown.
// First, tries to fetch at branch_or_commit.
// Next, try to get the branch of the developer's MR which triggered this run.
// Failing the above, get ToT.
// Finally, if testing a commit, it merges the commit being tested into ToT of the
// current main branch (which is either master or a release branch).
// Returns the short form commit hash of the commit being tested.
String getRepoCode(repo_url, branch_or_commit, merge_branch_or_commit, merge_output_log){
    refPrefix = "refs/heads/"
    gitlabRun = false

    if (branch_or_commit){
        branch_or_commit = branch_or_commit.trim()
    }

    echo("env.gitlabSourceBranch: " + env.gitlabSourceBranch)
    echo("branch_or_commit: " + branch_or_commit)
    echo("merge_branch_or_commit: " + merge_branch_or_commit)
    echo("merge_output_log: " + merge_output_log)

    if (branch_or_commit){
        // We don't know if this was a branch or a commit, so don't add the refPrefix.
        // Just insert exactly what the user requests.
        echo("Given a branch or commit, checking out " + branch_or_commit + " for repo " + repo_url)
        checkoutRepo(repo_url, branch_or_commit)
    }else if (isGitLabRun()){
        // When launched via gitlab triggering the pipeline plugin, there is a gitlabSourceBranch
        // environment variable.
        gitlabRun = true
        echo("Given a GitLab run, checking out " + env.gitlabSourceBranch + " for repo " + repo_url)
        checkoutRepo(repo_url, refPrefix + env.gitlabSourceBranch)
    }else{
        // This was launched some other way. Just get latest.
        echo("Not given a branch or commit, and not a GitLab run, checking out ToT for repo " + repo_url)
        checkoutRepo(repo_url, env.tot_branch)
    }

    commitBeingTested = getHead()

    if (gitlabRun){
        echo("Merging into ToT.")
        mergeToTot(env.gitlabSourceBranch, merge_output_log)
    }else if (merge_branch_or_commit && branch_or_commit){
        echo("Merging into ToT.")
        mergeToTot(branch_or_commit, merge_output_log)
    }else{
        echo("Not merging into ToT.")
    }

    return commitBeingTested
}

return this