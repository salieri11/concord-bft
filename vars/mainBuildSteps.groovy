import groovy.json.*

def call(){
  pipeline {
    agent any
    tools {
        nodejs 'Node 8.9.1'
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
            sh '''
              docker login -u athena-deployer -p "${ARTIFACTORY_PASSWORD}" athena-docker-local.artifactory.eng.vmware.com
            '''
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
            env.internal_repo = "athena-docker-local.artifactory.eng.vmware.com"

            // These are constants which mirror the DockerHub repos.  DockerHub is only used for publishing releases.
            env.release_concord_repo = env.release_repo + "/concord-core"
            env.release_helen_repo = env.release_repo + "/concord-ui"
            env.release_ethrpc_repo = env.release_repo + "/ethrpc"
            env.release_fluentd_repo = env.release_repo + "/fluentd"
            env.release_ui_repo = env.release_repo + "/ui"

            // These are constants which mirror the internal artifactory repos.  We put all merges
            // to master in the internal VMware artifactory.
            env.internal_concord_repo = env.release_concord_repo.replace(env.release_repo, env.internal_repo)
            env.internal_helen_repo = env.internal_repo + "/helen"
            env.internal_ethrpc_repo = env.release_ethrpc_repo.replace(env.release_repo, env.internal_repo)
            env.internal_fluentd_repo = env.release_fluentd_repo.replace(env.release_repo, env.internal_repo)
            env.internal_ui_repo = env.release_ui_repo.replace(env.release_repo, env.internal_repo)
          }

          // Docker-compose picks up values from the .env file in the directory from which
          // docker-compose is run.
          withCredentials([string(credentialsId: 'BUILDER_ACCOUNT_PASSWORD', variable: 'PASSWORD')]) {
            sh '''
              echo "${PASSWORD}" | sudo -S ls
              sudo cat >blockchain/concord/docker/.env <<EOF
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
EOF
              cp blockchain/concord/docker/.env blockchain/hermes/
            '''
          }

          // These are constants related to labelling Docker images.
          script {
            env.version_label = 'com.vmware.blockchain.version'
            env.commit_label = 'com.vmware.blockchain.commit'
          }
        }
      }

      stage('Build products') {
        stages {
          stage('Build Concord') {
            steps {
              dir('blockchain/concord') {
                sh '''currentDir=`pwd`
                sed -i\'\' "s?./test/resources/genesis.json?${currentDir}/test/resources/genesis.json?g" test/resources/concord1.config
                sed -i\'\' "s?./test/resources/genesis.json?${currentDir}/test/resources/genesis.json?g" test/resources/concord2.config
                sed -i\'\' "s?./test/resources/genesis.json?${currentDir}/test/resources/genesis.json?g" test/resources/concord3.config
                sed -i\'\' "s?./test/resources/genesis.json?${currentDir}/test/resources/genesis.json?g" test/resources/concord4.config

                git submodule init
                git submodule update --recursive
                mkdir -p build
                cd build
                cmake ..
                make'''
              }
            }
          }
        }
      }

      stage("Build docker images") {
        parallel {
          stage("Build helen docker image") {
            steps {
              script {
                dir('blockchain/helen') {
                  withCredentials([string(credentialsId: 'BUILDER_ACCOUNT_PASSWORD', variable: 'PASSWORD')]) {
                    sh '''
                      docker build .. -f Dockerfile -t "${internal_helen_repo}:${docker_tag}" --label ${version_label}=${docker_tag} --label ${commit_label}=${actual_blockchain_fetched}
                    '''
                  }
                }
              }
            }
          }

          stage("Build ethrpc docker image") {
            steps {
              script {
                dir('blockchain/ethrpc') {
                  withCredentials([string(credentialsId: 'BUILDER_ACCOUNT_PASSWORD', variable: 'PASSWORD')]) {
                    sh '''
                      docker build .. -f Dockerfile -t "${internal_ethrpc_repo}:${docker_tag}" --label ${version_label}=${docker_tag} --label ${commit_label}=${actual_blockchain_fetched}
                    '''
                    }
                }
              }
            }
          }

          stage('Build concord docker image') {
            steps {
              script {
                dir('blockchain/concord') {
                  withCredentials([string(credentialsId: 'BLOCKCHAIN_REPOSITORY_WRITER_PWD', variable: 'DOCKERHUB_PASSWORD')]) {
                    sh '''
                      docker build .. -f Dockerfile -t "${internal_concord_repo}:${docker_tag}" --label ${version_label}=${docker_tag} --label ${commit_label}=${actual_blockchain_fetched}
                    '''
                  }
                }
              }
            }
          }

          stage('Build fluentd docker image') {
            steps {
              script {
                dir('blockchain/concord/docker') {
                  withCredentials([string(credentialsId: 'BLOCKCHAIN_REPOSITORY_WRITER_PWD', variable: 'DOCKERHUB_PASSWORD')]) {
                    sh '''
                      docker-compose build fluentd
                    '''
                  }
                }
              }
            }
          }

          stage('Build ui docker image') {
            steps {
              script {
                dir('blockchain/concord/docker') {
                  withCredentials([string(credentialsId: 'BLOCKCHAIN_REPOSITORY_WRITER_PWD', variable: 'DOCKERHUB_PASSWORD')]) {
                    sh '''
                      docker-compose build ui
                    '''
                  }
                }
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
                env.beerwars_test_logs = new File(env.test_log_root, "BeerWars").toString()
                env.core_vm_test_logs = new File(env.test_log_root, "CoreVM").toString()
                env.helen_api_test_logs = new File(env.test_log_root, "HelenAPI").toString()
                env.extended_rpc_test_logs = new File(env.test_log_root, "ExtendedRPC").toString()
                env.regression_test_logs = new File(env.test_log_root, "Regression").toString()
                env.statetransfer_test_logs = new File(env.test_log_root, "StateTransfer").toString()
              }

              sh '''
                # We need to delete the database files before running UI tests because
                # Selenium cannot launch Chrome with sudo.  (The only reason Hermes
                # needs to be run with sudo is so it can delete any existing DB files.)
                echo "${PASSWORD}" | sudo -S rm -rf ../concord/docker/rocksdbdata*
                echo "${PASSWORD}" | sudo -S rm -rf ../concord/docker/cockroachDB
                ./main.py UiTests --dockerComposeFile ../concord/docker/docker-compose.yml --resultsDir "${ui_test_logs}"

                echo "${PASSWORD}" | sudo -S ./main.py BeerWarsTests --dockerComposeFile ../concord/docker/docker-compose.yml --resultsDir "${beerwars_test_logs}"
                echo "${PASSWORD}" | sudo -S ./main.py CoreVMTests --dockerComposeFile ../concord/docker/docker-compose.yml --resultsDir "${core_vm_test_logs}"
                echo "${PASSWORD}" | sudo -S ./main.py HelenAPITests --dockerComposeFile ../concord/docker/docker-compose.yml --resultsDir "${helen_api_test_logs}"
                echo "${PASSWORD}" | sudo -S ./main.py ExtendedRPCTests --dockerComposeFile ../concord/docker/docker-compose.yml --resultsDir "${extended_rpc_test_logs}"
                echo "${PASSWORD}" | sudo -S ./main.py RegressionTests --dockerComposeFile ../concord/docker/docker-compose.yml --resultsDir "${regression_test_logs}"
                echo "${PASSWORD}" | sudo -S ./main.py SimpleStateTransferTest --dockerComposeFile ../concord/docker/docker-compose.yml --resultsDir "${statetransfer_test_logs}"
              '''
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
          withCredentials([string(credentialsId: 'ATHENA_DEPLOYER_ARTIFACTORY_PASSWORD', variable: 'ARTIFACTORY_PASSWORD')]) {
            sh '''
              docker login -u athena-deployer -p "${ARTIFACTORY_PASSWORD}" athena-docker-local.artifactory.eng.vmware.com
            '''
          }

          script {
            // Pass in false for whether to tag as latest because VMware's
            // artifactory does not allow re-using a tag.
            pushDockerImage(env.internal_concord_repo, env.docker_tag, false)
            pushDockerImage(env.internal_helen_repo, env.docker_tag, false)
            pushDockerImage(env.internal_ethrpc_repo, env.docker_tag, false)
            pushDockerImage(env.internal_fluentd_repo, env.docker_tag, false)
            pushDockerImage(env.internal_ui_repo, env.docker_tag, false)
          }

          sh '''
            docker logout
          '''
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
              docker logout
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
            '''
            pushDockerImage(env.release_concord_repo, env.docker_tag, true)
            pushDockerImage(env.release_helen_repo, env.docker_tag, true)
            pushDockerImage(env.release_ethrpc_repo, env.docker_tag, true)
            pushDockerImage(env.release_fluentd_repo, env.docker_tag, true)
            pushDockerImage(env.release_ui_repo, env.docker_tag, true)
          }

          sh '''
            docker logout
          '''

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
  result = sh (
    script: "docker push ${repo}:${tag}",
  )

  if(tagAsLatest){
    sh(
      script: "docker tag ${repo}:${tag} ${repo}:latest && docker push ${repo}:latest",
    )
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
