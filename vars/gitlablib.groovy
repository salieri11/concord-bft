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

return this