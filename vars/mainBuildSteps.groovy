import groovy.json.*

def call(){
  def agentLabel = "genericVM"
  def genericTests = true
  def memory_leak_job_name = "BlockchainMemoryLeakTesting"
  def performance_test_job_name = "Blockchain Performance Test"

  if (env.JOB_NAME == memory_leak_job_name) {
    echo "**** Jenkins job for Memory Leak Test"
    agentLabel = "MemoryLeakTesting"
    genericTests = false
  } else if (env.JOB_NAME == performance_test_job_name) {
    echo "**** Jenkins job for Performance Test"
    genericTests = false
  } else {
    echo "**** Jenkins job for Generic Test Run"
  }

  pipeline {
    agent { label agentLabel }
    tools {
        // 8.9.4 is the minimum for Truffle.
        nodejs 'Node 8.9.4'
    }
    parameters {
      booleanParam defaultValue: false, description: "Whether to deploy the docker images for production. REQUIRES A VERSION NUMBER IN THE 'version_param' FIELD.", name: "deploy"
      string defaultValue: "",
             description: "The version number for releases. Used as a tag in DockerHub and GitHub.  REQUIRED IF THE 'deploy' CHECKBOX IS CHECKED.",
             name: "version_param"

      string defaultValue: "",
             description: "Blockchain commit or branch to use.  Providing a branch name will pull the branch's latest commit.",
             name: "blockchain_branch_or_commit"
      string defaultValue: "",
             description: "Shared Jenkins lib branch to use.",
             name: "shared_lib_branch"
    }
    stages {
      stage("Display node info"){
        steps{
          sh '''
            set +x
            echo Jenkins node information:
            ifconfig | grep -A 2 "ens"
            set -x
          '''
        }
      }

      stage("Check parameters"){
        steps{
          script{
            errString = "Parameter check error: "

            if (params.deploy && (!params.version_param || !params.version_param.trim())){
              throw new Exception (errString + "A version number must be entered when the 'deploy' checkbox is checked.")
            }
          }
        }
      }

      stage("Clean") {
        steps {
          cleanWs()
        }
      }

      stage('Fetch source code') {
        parallel {
          stage("Fetch blockchain repo source") {
            steps {
              sh 'mkdir blockchain'
              dir('blockchain') {
                script {
                  env.commit = getRepoCode("git@github.com:vmwathena/blockchain.git", params.blockchain_branch_or_commit)
                }
              }
            }
          }
          stage('Fetch VMware blockchain hermes-data source') {
            steps {
              sh 'mkdir hermes-data'
              dir('hermes-data') {
                script {
                  env.actual_hermes_data_fetched = getRepoCode("git@github.com:vmwathena/hermes-data","master")
                }
                sh 'git checkout master'
              }
            }
          }
        }
      }

      stage("Copy dependencies") {
        parallel {
          stage("Copy googletest") {
            steps() {
              sh 'mkdir googletest'
              dir('googletest') {
                sh 'cp -ar /var/jenkins/workspace/googletest/* .'
              }
            }
          }
          stage("Copy evmjit") {
            steps() {
              sh 'mkdir evmjit'
              dir('evmjit') {
                sh 'cp -ar /var/jenkins/workspace/evmjit/* .'
              }
            }
          }
          stage("Copy etherium tests") {
            steps() {
              sh 'mkdir ethereum_tests'
              dir('ethereum_tests') {
                sh 'cp -ar /var/jenkins/workspace/ethereum_tests/* .'
              }
            }
          }
          stage("Install node package dependencies") {
            steps() {
              dir('blockchain/ui') {
                sh 'npm install'
              }
            }
          }
        }
      }

      stage('Write version for GUI') {
        steps() {
          dir('blockchain') {
            script {
              version = env.version_param ? env.version_param : env.commit
              env.version_json = createVersionInfo(version, env.commit)
            }
            // The groovy calls to create directories and files fail, only in Jenkins,
            // so do those in a shell block.  Jenkins has some quirky ideas of security?
            sh '''
              dir=ui/src/static/data
              mkdir -p ${dir}
              echo ${version_json} > ${dir}/version.json
            '''
          }
        }
      }

      stage("Configure docker and git") {
        steps {
          // Docker will fail to launch unless we fix up this DNS stuff.  It will try to use Google's
          // DNS servers by default, and here in VMware's network, we can't do that.
          // Also, since this will run on a VM which may have been deployed anywhere in the world,
          // do not hard code the DNS values.  Always probe the current environment and write
          // this file.
          // Reference: https://development.robinwinslow.uk/2016/06/23/fix-docker-networking-dns/
          withCredentials([string(credentialsId: 'BUILDER_ACCOUNT_PASSWORD', variable: 'PASSWORD')]) {
            sh '''
              DNS_JSON_STRING=$(echo {\\"dns\\": [\\"`nmcli dev show | grep 'IP4.DNS' | cut --delimiter=':' --fields=2 | xargs | sed 's/\\s/", "/g'`\\"]})
              echo "${PASSWORD}" | sudo -S ls > /dev/null
              echo $DNS_JSON_STRING | sudo tee -a /etc/docker/daemon.json
              sudo service docker restart
            '''
          }

          withCredentials([string(credentialsId: 'ATHENA_DEPLOYER_ARTIFACTORY_PASSWORD', variable: 'ARTIFACTORY_PASSWORD')]) {
            script{
              command = "docker login -u athena-deployer -p \"" + env.ARTIFACTORY_PASSWORD + "\" athena-docker-local.artifactory.eng.vmware.com"
              retryCommand(command, true)
            }
          }

          // To invoke "git tag" and commit that change, git wants to know who we are.
          // This will be set up in template VM version 5, at which point these commands can
          // be removed.
          sh '''
            git config --global user.email "vmwathenabot@vmware.com"
            git config --global user.name "build system"
          '''

          // Set up repo variables.
          script {
            env.docker_tag = env.version_param ? env.version_param : env.commit
            env.release_repo = "vmwblockchain"
            env.internal_repo_name = "athena-docker-local"
            env.internal_repo = env.internal_repo_name + ".artifactory.eng.vmware.com"

            // These are constants which mirror the DockerHub repos.  DockerHub is only used for publishing releases.
            env.release_concord_repo = env.release_repo + "/concord-core"
            env.release_helen_repo = env.release_repo + "/concord-ui"
            env.release_ethrpc_repo = env.release_repo + "/ethrpc"
            env.release_fluentd_repo = env.release_repo + "/fluentd"
            env.release_ui_repo = env.release_repo + "/ui"
            env.release_asset_transfer_repo = env.release_repo + "/asset-transfer"
            env.release_agent_repo = env.release_repo + "/agent"

            // These are constants which mirror the internal artifactory repos.  We put all merges
            // to master in the internal VMware artifactory.
            env.internal_concord_repo = env.release_concord_repo.replace(env.release_repo, env.internal_repo)
            env.internal_helen_repo = env.internal_repo + "/helen"
            env.internal_ethrpc_repo = env.release_ethrpc_repo.replace(env.release_repo, env.internal_repo)
            env.internal_fluentd_repo = env.release_fluentd_repo.replace(env.release_repo, env.internal_repo)
            env.internal_ui_repo = env.release_ui_repo.replace(env.release_repo, env.internal_repo)
            env.internal_asset_transfer_repo = env.release_asset_transfer_repo.replace(env.release_repo, env.internal_repo)
            env.internal_agent_repo = env.release_agent_repo.replace(env.release_repo, env.internal_repo)
          }

          // Docker-compose picks up values from the .env file in the directory from which
          // docker-compose is run.
          withCredentials([
            string(credentialsId: 'BUILDER_ACCOUNT_PASSWORD', variable: 'PASSWORD'),
            string(credentialsId: 'LINT_API_KEY', variable: 'LINT_API_KEY'),
            string(credentialsId: 'FLUENTD_AUTHORIZATION_BEARER', variable: 'FLUENTD_AUTHORIZATION_BEARER')
            ]) {
            sh '''
              echo "${PASSWORD}" | sudo -S ls
              sudo cat >blockchain/docker/.env <<EOF
concord_repo=${internal_concord_repo}
concord_tag=${docker_tag}
helen_repo=${internal_helen_repo}
helen_tag=${docker_tag}
ethrpc_repo=${internal_ethrpc_repo}
ethrpc_tag=${docker_tag}
fluentd_repo=${internal_fluentd_repo}
fluentd_tag=${docker_tag}
ui_repo=${internal_ui_repo}
ui_tag=${docker_tag}
asset_transfer_repo=${internal_asset_transfer_repo}
asset_transfer_tag=${docker_tag}
agent_repo=${internal_agent_repo}
agent_tag=${docker_tag}
commit_hash=${commit}
LINT_API_KEY=${LINT_API_KEY}
EOF
              cp blockchain/docker/.env blockchain/hermes/

              # Need to add the fluentd authorization bearer.
              # I couldn't get this env method to update the conf https://docs.fluentd.org/v0.12/articles/faq#how-can-i-use-environment-variables-to-configure-parameters-dynamically
              sed -i -e 's/'"<ADD-LOGINTELLIGENCE-KEY-HERE>"'/'"${FLUENTD_AUTHORIZATION_BEARER}"'/g' blockchain/docker/fluentd/fluentd.conf
            '''
          }
        }
      }

      stage("Build") {
        steps {
          dir('blockchain') {
            sh '''
              ./buildall.sh
            '''
          }
        }
      }

      stage ("Build Performance Sub module") {
        when {
          expression { env.JOB_NAME == performance_test_job_name }
        }
        stages {
          stage('Maven build... mvn_performance_build.log') {
            steps {
              dir('blockchain/performance') {
                  sh 'mvn clean install assembly:single > mvn_performance_build.log 2>&1'
              }
            }
          }
        }
      }

      stage("Run tests in containers") {
        steps {
          dir('blockchain/hermes'){
            withCredentials([string(credentialsId: 'BUILDER_ACCOUNT_PASSWORD', variable: 'PASSWORD')]) {
              script {
                env.test_log_root = new File(env.WORKSPACE, "testLogs").toString()
                env.ui_test_logs = new File(env.test_log_root, "UI").toString()
                env.asset_transfer_test_logs = new File(env.test_log_root, "AssetTransfer").toString()
                env.core_vm_test_logs = new File(env.test_log_root, "CoreVM").toString()
                env.helen_api_test_logs = new File(env.test_log_root, "HelenAPI").toString()
                env.extended_rpc_test_logs = new File(env.test_log_root, "ExtendedRPC").toString()
                env.regression_test_logs = new File(env.test_log_root, "Regression").toString()
                env.statetransfer_test_logs = new File(env.test_log_root, "StateTransfer").toString()
                env.truffle_test_logs = new File(env.test_log_root, "Truffle").toString()
                env.mem_leak_test_logs = new File(env.test_log_root, "MemoryLeak").toString()
                env.performance_test_logs = new File(env.test_log_root, "PerformanceTest").toString()

                if (genericTests) {
                  sh '''
                    # So test suites not using sudo can write to test_logs.
                    mkdir "${test_log_root}"

                    echo "${PASSWORD}" | sudo -S ./main.py AssetTransferTests --dockerComposeFile ../docker/docker-compose.yml --resultsDir "${asset_transfer_test_logs}"
                    echo "${PASSWORD}" | sudo -S ./main.py CoreVMTests --dockerComposeFile ../docker/docker-compose.yml --resultsDir "${core_vm_test_logs}"
                    echo "${PASSWORD}" | sudo -S ./main.py HelenAPITests --dockerComposeFile ../docker/docker-compose.yml --resultsDir "${helen_api_test_logs}"
                    echo "${PASSWORD}" | sudo -S ./main.py ExtendedRPCTests --dockerComposeFile ../docker/docker-compose.yml --resultsDir "${extended_rpc_test_logs}"
                    echo "${PASSWORD}" | sudo -S ./main.py RegressionTests --dockerComposeFile ../docker/docker-compose.yml --resultsDir "${regression_test_logs}"
                    echo "${PASSWORD}" | sudo -S ./main.py SimpleStateTransferTest --dockerComposeFile ../docker/docker-compose.yml --resultsDir "${statetransfer_test_logs}"
                    echo "${PASSWORD}" | sudo -S ./main.py TruffleTests --logLevel debug --dockerComposeFile ../docker/docker-compose.yml --resultsDir "${truffle_test_logs}"

                    cd suites ; echo "${PASSWORD}" | sudo -SE ./memory_leak_test.sh --testSuite CoreVMTests --repeatSuiteRun 2 --tests vmArithmeticTest/add0.json --resultsDir ${mem_leak_test_logs} ; cd ..

                    # We need to delete the database files before running UI tests because
                    # Selenium cannot launch Chrome with sudo.  (The only reason Hermes
                    # needs to be run with sudo is so it can delete any existing DB files.)
                    echo "${PASSWORD}" | sudo -S rm -rf ../docker/devdata/rocksdbdata*
                    echo "${PASSWORD}" | sudo -S rm -rf ../docker/devdata/cockroachDB
                    ./main.py UiTests --dockerComposeFile ../docker/docker-compose.yml ../docker/docker-compose-fluentd.yml --resultsDir "${ui_test_logs}"
                  '''
                }
                if (env.JOB_NAME == memory_leak_job_name) {
                  sh '''
                    echo "Running Entire Testsuite: Memory Leak..."
                    cd suites ; echo "${PASSWORD}" | sudo -SE ./memory_leak_test.sh --testSuite CoreVMTests --repeatSuiteRun 5 --resultsDir ${mem_leak_test_logs}
                  '''
                }
                if (env.JOB_NAME == performance_test_job_name) {
                  sh '''
                    echo "Running Entire Testsuite: Performance..."
                    echo "${PASSWORD}" | sudo -S ./main.py PerformanceTests --dockerComposeFile ../docker/docker-compose.yml --resultsDir "${performance_test_logs}"
                  '''
                }
              }
            }
          }
        }
      }

      stage ("Post Memory Leak Testrun") {
        when {
          expression { env.JOB_NAME == memory_leak_job_name }
        }
        stages {
          stage('Push memory leak summary into repo') {
            steps {
              dir('hermes-data/memory_leak_test') {
                  pushMemoryLeakSummary()
              }
            }
          }
          stage ('Send Memory Leak Alert Notification') {
            steps {
                dir('hermes-data/memory_leak_test') {
                    script {
                        memory_leak_spiked_log = new File(env.mem_leak_test_logs, "memory_leak_spiked.log").toString()
                        if (fileExists(memory_leak_spiked_log)) {
                            echo 'ALERT: Memory Leak spiked up'

                            memory_leak_alert_notification_address_file = "memory_leak_alert_recipients.txt"
                            if (fileExists(memory_leak_alert_notification_address_file)) {
                                memory_leak_alert_notification_recipients = readFile(memory_leak_alert_notification_address_file).replaceAll("\n", " ")
                                echo 'Sending ALERT email notification...'
                                emailext body: "Memory Leak Spiked up in build: ${env.BUILD_NUMBER}\n\n More info at: ${env.BUILD_URL}\nDownload Valgrind Log file (could be > 10 MB): ${env.BUILD_URL}artifact/testLogs/MemoryLeak/valgrind_concord1.log\n\nGraph: ${JOB_URL}plot",
                                to: memory_leak_alert_notification_recipients,
                                subject: "ALERT: Memory Leak Spiked up in build ${env.BUILD_NUMBER}"
                            }
                        }
                    }
                }
            }
          }
          stage ('Graph') {
            steps {
              plot csvFileName: 'plot-leaksummary.csv',
                csvSeries: [[
                            file: 'memory_leak_summary.csv',
                            exclusionValues: '',
                            displayTableFlag: false,
                            inclusionFlag: 'OFF',
                            url: '']],
              group: 'Memory Leak',
              title: 'Memory Leak Summary',
              style: 'line',
              exclZero: false,
              keepRecords: false,
              logarithmic: false,
              numBuilds: '',
              useDescr: false,
              yaxis: 'Leak Summary (bytes)',
              yaxisMaximum: '',
              yaxisMinimum: ''
            }
          }
        }
      }

      stage("Save to artifactory"){
        when {
          expression {
            return JOB_NAME == "Blockchain Master/master"
          }
        }
        steps{
          script {
            withCredentials([string(credentialsId: 'ARTIFACTORY_API_KEY', variable: 'ARTIFACTORY_API_KEY')]) {
              // Pass in false for whether to tag as latest because VMware's
              // artifactory does not allow re-using a tag.
              pushDockerImage(env.internal_concord_repo, env.docker_tag, false)
              pushDockerImage(env.internal_helen_repo, env.docker_tag, false)
              pushDockerImage(env.internal_ethrpc_repo, env.docker_tag, false)
              pushDockerImage(env.internal_fluentd_repo, env.docker_tag, false)
              pushDockerImage(env.internal_ui_repo, env.docker_tag, false)
              pushDockerImage(env.internal_asset_transfer_repo, env.docker_tag, false)
              pushDockerImage(env.internal_agent_repo, env.docker_tag, false)
            }
          }
        }
      }

      stage("Release") {
        when {
          environment name: 'deploy', value: 'true'
        }
        steps {
          dir('blockchain') {
            createAndPushGitTag(env.version_param)
          }

          withCredentials([string(credentialsId: 'BLOCKCHAIN_REPOSITORY_WRITER_PWD', variable: 'DOCKERHUB_PASSWORD')]) {
            sh '''
              docker login -u blockchainrepositorywriter -p "${DOCKERHUB_PASSWORD}"
            '''
          }

          script {
            sh '''
              docker tag ${internal_concord_repo}:${docker_tag} ${release_concord_repo}:${docker_tag}
              docker tag ${internal_helen_repo}:${docker_tag} ${release_helen_repo}:${docker_tag}
              docker tag ${internal_ethrpc_repo}:${docker_tag} ${release_ethrpc_repo}:${docker_tag}
              docker tag ${internal_fluentd_repo}:${docker_tag} ${release_fluentd_repo}:${docker_tag}
              docker tag ${internal_ui_repo}:${docker_tag} ${release_ui_repo}:${docker_tag}
              docker tag ${internal_asset_transfer_repo}:${docker_tag} ${release_asset_transfer_repo}:${docker_tag}
              docker tag ${internal_agent_repo}:${docker_tag} ${release_agent_repo}:${docker_tag}
            '''
            pushDockerImage(env.release_concord_repo, env.docker_tag, true)
            pushDockerImage(env.release_helen_repo, env.docker_tag, true)
            pushDockerImage(env.release_ethrpc_repo, env.docker_tag, true)
            pushDockerImage(env.release_fluentd_repo, env.docker_tag, true)
            pushDockerImage(env.release_ui_repo, env.docker_tag, true)
            pushDockerImage(env.release_asset_transfer_repo, env.docker_tag, true)
            pushDockerImage(env.release_agent_repo, env.docker_tag, true)
          }

          dir('blockchain/vars') {
            script {
              release_notification_address_file = "release_notification_recipients.txt"

              if (fileExists(release_notification_address_file)) {
                release_notification_recipients = readFile(release_notification_address_file).replaceAll("\n", " ")
                emailext body: "Changes: \n" + getChangesSinceLastTag(),
                     to: release_notification_recipients,
                     subject: "[Build] Concord version " + env.version_param + " has been pushed to DockerHub."
              }
            }
          }
        }
      }
    }// End stages

    post {
      always {
        script{
          command = "docker logout"
          retryCommand(command, false)

          command = "docker logout athena-docker-local.artifactory.eng.vmware.com"
          retryCommand(command, false)
        }

        // Files created by the docker run belong to root because they were created by the docker process.
        // That will make the subsequent run unable to clean the workspace.  Just make the entire workspace dir
        // belong to builder to catch any future files.
        dir(env.WORKSPACE){
          withCredentials([string(credentialsId: 'BUILDER_ACCOUNT_PASSWORD', variable: 'PASSWORD')]) {
            sh '''
              echo "${PASSWORD}" | sudo -S chown -R builder:builder .
            '''
          }
        }

        archiveArtifacts artifacts: "**/*.log", allowEmptyArchive: true
        archiveArtifacts artifacts: "**/*.json", allowEmptyArchive: true
        archiveArtifacts artifacts: "**/*.html", allowEmptyArchive: true

        echo 'Sending email notification...'
        emailext body: "${currentBuild.currentResult}: Job ${env.JOB_NAME} build ${env.BUILD_NUMBER}\n More info at: ${env.BUILD_URL}",
        recipientProviders: [[$class: 'DevelopersRecipientProvider'], [$class: 'RequesterRecipientProvider']],
        subject: "Jenkins Build ${currentBuild.currentResult}: Job ${env.JOB_NAME}"

      }
    }
  }
}

// The user's parameter is top priority, and if it fails, let an exception be thrown.
// First, tries to fetch at branch_or_commit.
// Next, get master.
// Next, try to get BRANCH_NAME.  If getting BRANCH_NAME fails, we are probably testing
// a branch that is in only in one or two of the repos.  That's fine.
// Returns the short form commit hash.
String getRepoCode(repo_url, branch_or_commit){
  refPrefix = "refs/heads/"

  if (branch_or_commit && branch_or_commit.trim()){
    // We don't know if this was a branch or a commit, so don't add the refPrefix.
    checkoutRepo(repo_url, branch_or_commit)
  }else if (env.BRANCH_NAME && env.BRANCH_NAME.trim()){
    // When launched via the multibranch pipeline plugin, there is a BRANCH_NAME
    // environment variable.
    checkoutRepo(repo_url, refPrefix + env.BRANCH_NAME)
  }else{
    // This was launched some other way. Just get latest.
    checkoutRepo(repo_url, "master")
  }

  return sh (
    script: 'git rev-parse --short HEAD',
    returnStdout: true
  ).trim()
}

// All that varies for each repo is the branch, so wrap this very large call.
void checkoutRepo(repo_url, branch_or_commit){
  checkout([$class: 'GitSCM', branches: [[name: branch_or_commit]], doGenerateSubmoduleConfigurations: false, extensions: [[$class: 'SubmoduleOption', disableSubmodules: false, parentCredentials: true, recursiveSubmodules: true, reference: '', trackingSubmodules: false]], submoduleCfg: [], userRemoteConfigs: [[credentialsId: '27bbd815-703c-4647-909b-836919db98ef', url: repo_url]]])
}

// Given a repo and tag, pushes a docker image to whatever repo we
// are currently logged into (set up by the caller).  If tagAsLatest
// is set to true, that image will be re-tagged as latest and pushed
// again.
void pushDockerImage(repo, tag, tagAsLatest){
  if (repo.contains(env.internal_repo_name)){
    // Re-pushing to artifactory will trigger an error.
    component = repo.split("eng.vmware.com")[1]
    apiLookupString = env.internal_repo_name + component + "/" + tag

    if (existsInArtifactory(apiLookupString)){
      return
    }
  }

  retryCommand("docker push ${repo}:${tag}", true)

  if(tagAsLatest){
    command = "docker tag ${repo}:${tag} ${repo}:latest && docker push ${repo}:latest"
    retryCommand(command, true)
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

// Use groovy to create and return json for the version and commit
// for this run.
void createVersionInfo(version, commit){
  versionObject = [:]
  versionObject.version = version
  versionObject.commit = commit
  return new JsonOutput().toJson(versionObject)
}

void pushMemoryLeakSummary(){
  echo "git add"
  sh (
    script: "git add memory_leak_summary.csv",
    returnStdout: false
  )
  echo "git commit"
  sh (
    script: "git commit -m 'Update memory leak summary data'",
    returnStdout: false
  )

  echo "git push"
  sh (
    script: "git push origin master",
    returnStdout: false
  )
}

// Uses the artifactory REST API to return whether the passed in object
// exists in the VMware artifactory. The passed in object is the path
// seen in the Artifactory GUI.  e.g.
// athena-docker-local/test/concord-core/2ef3010
Boolean existsInArtifactory(String path){
  echo "Checking for existence of '" + path + "' in the VMware artifactory"
  found = false
  baseUrl = "https://build-artifactory.eng.vmware.com/artifactory/api/storage/"
  resultJsonFile = "artifactoryResult.json"
  curlCommand = "curl -s -H 'X-JFrog-Art-Api: " + env.ARTIFACTORY_API_KEY + "' " + baseUrl + path
  curlCommand += " -o " + resultJsonFile
  retryCurl(curlCommand, true)

  // If it is there, we get a structure like this:
  // {
  //   "repo" : "athena-docker-local",
  //   "path" : "/test/concord-core/2ef3010",
  //   ...
  //
  // If not, we get:
  // {
  //   "errors" : [ {
  //     "status" : 404,
  //     "message" : "Unable to find item"
  //   } ]
  // }

  resultJson = readFile(resultJsonFile)
  resultObj = new JsonSlurperClassic().parseText(resultJson)

  if (resultObj.path){
    echo "Found " + path
    return true
  }else{
    echo "Did not find " + path
    return false
  }
}

// Given a command, execute it, retrying a few
// times if there is an error. Returns true if
// the command succeeds.  If the command fails:
//   - Raises an exception if failOnError is true.
//   - Returns false if failOnError is false.
// DO NOT USE THIS DIRECTLY FOR CURL, as curl exits with 0
// for cases we would want to retry.
Boolean retryCommand(command, failOnError){
  tries = 0
  maxTries = 10
  sleepTime = 10

  while (tries < maxTries){
    tries += 1

    status = sh(
      script: command,
      returnStatus: true
    )

    if (status == 0){
      return true
    }else{
      echo "Command '" + command + "' failed."

      if (tries < maxTries){
        echo "Retrying in " + sleepTime + " seconds."
        sleep(sleepTime)
      }
    }
  }

  msg = "Failed to run the command '" + command + "'."

  if(failOnError){
    error(msg)
  }else{
    echo(msg)
    return false
  }
}

// Given a curl command (without the --dump-header parameter),
// run it, and retry if the header indicates a problem.
// Returns true if the command succeeds.  If the command fails:
//   - Raises an exception if failOnError is true.
//   - Returns false if failOnError is false.
Boolean retryCurl(command, failOnError){
  tries = 0
  maxTries = 10
  sleepTime = 10
  headerFile = "header.txt"
  command += " --dump-header " + headerFile

  sh(script: "rm -f " + headerFile)

  while (tries < maxTries){
    tries += 1

    // The retryCommand function will repeat until we get
    // a nonzero exit code, which covers things like a typo
    // in the protocol or the server not responding at all.
    commandResult = retryCommand(command, failOnError)

    if(!commandResult){
      return false
    }else{
      headers = readFile(headerFile)
      statusHeader = headers.readLines()[0]
      statusCode = statusHeader.split(" ")[1]

      if(statusCode == "500" || statusCode == "503"){
        echo "Attempt " + tries + " of command '" + command + "', returned status: '" + statusHeader + "'"

        if(tries < maxTries){
          echo "Retrying in " + sleepTime + " seconds"
          sleep(sleepTime)
        }
      }
      else{
        return true
      }
    }
  }

  msg = "Failed to run '" + command + "'."

  if(failOnError){
    error(msg)
  }else{
    echo(msg)
    return false
  }
}
