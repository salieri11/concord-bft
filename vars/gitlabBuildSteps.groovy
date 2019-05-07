import groovy.json.*

def call(){
  def agentLabel = "genericVM"
  def genericTests = true
  def additional_components_to_build = ""
  def master_branch_job_name = "Master Branch"
  def lint_test_job_name = "Blockchain LINT Tests"
  def memory_leak_job_name = "BlockchainMemoryLeakTesting"
  def performance_test_job_name = "Blockchain Performance Test"
  def persephone_test_job_name = "Blockchain Persephone Tests"

  if (env.JOB_NAME.contains(memory_leak_job_name)) {
    echo "**** Jenkins job for Memory Leak Test"
    agentLabel = "MemoryLeakTesting"
    genericTests = false
  } else if (env.JOB_NAME.contains(performance_test_job_name)) {
    echo "**** Jenkins job for Performance Test"
    genericTests = false
    additional_components_to_build = additional_components_to_build + "PerformanceTests,"
  } else if (env.JOB_NAME.contains(persephone_test_job_name)) {
    echo "**** Jenkins job for Persephone Test"
    genericTests = false
  } else if (env.JOB_NAME.contains(lint_test_job_name)) {
    echo "**** Jenkins job for LINT Tests"
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
    options{
      gitLabConnection('TheGitlabConnection')
    }
    parameters {
      booleanParam defaultValue: false, description: "Whether to deploy the docker images for production. REQUIRES A VERSION NUMBER IN THE 'version_param' FIELD.", name: "deploy"
      string defaultValue: "",
             description: "The version number for releases. Used as a tag in DockerHub and Git.  REQUIRED IF THE 'deploy' CHECKBOX IS CHECKED.",
             name: "version_param"

      string defaultValue: "",
             description: "Blockchain commit or branch to use.  Providing a branch name will pull the branch's latest commit.",
             name: "blockchain_branch_or_commit"
      string defaultValue: "",
             description: "Shared Jenkins lib branch to use.",
             name: "shared_lib_branch"
    }
    stages {
      stage("Notify GitLab"){
        steps{
          startRun()
        }
      }

      stage("Setup"){
        steps{
          script{
            try{
              // Output node info
              sh '''
                set +x
                echo Jenkins node information:
                ifconfig | grep -A 2 "ens"
                set -x
              '''

              // Check parameters
              script{
                errString = "Parameter check error: "

                if (params.deploy && (!params.version_param || !params.version_param.trim())){
                  throw new Exception (errString + "A version number must be entered when the 'deploy' checkbox is checked.")
                }
              }

              // Clean the workspace
              cleanWs()

              // Add the VMware GitLab ssh key to known_hosts.
              handleKnownHosts("gitlab.eng.vmware.com")
            }catch(Exception ex){
              failRun()
              throw ex
            }
          }
        }
      }

      stage('Fetch source code') {
        parallel {
          stage("Fetch blockchain repo source") {
            steps {
              script{
                try{
                  sh 'mkdir blockchain'
                  dir('blockchain') {
                    script {
                      env.commit = getRepoCode("git@gitlab.eng.vmware.com:blockchain/vmwathena_blockchain.git", params.blockchain_branch_or_commit)
                    }
                  }
                }catch(Exception ex){
                  failRun()
                  throw ex
                }
              }
            }
          }

          stage('Fetch VMware blockchain hermes-data source') {
            steps {
              script{
                try{
                  sh 'mkdir hermes-data'
                  dir('hermes-data') {
                    script {
                      env.actual_hermes_data_fetched = getRepoCode("git@gitlab.eng.vmware.com:blockchain/hermes-data","master")
                    }
                    sh 'git checkout master'
                  }
                }catch(Exception ex){
                  failRun()
                  throw ex
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
              script{
                try{
                  sh 'mkdir googletest'
                  dir('googletest') {
                    sh 'cp -ar /var/jenkins/workspace/googletest/* .'
                  }
                }catch(Exception ex){
                  failRun()
                  throw ex
                }
              }
            }
          }
          stage("Copy evmjit") {
            steps() {
              script{
                try{
                  sh 'mkdir evmjit'
                  dir('evmjit') {
                    sh 'cp -ar /var/jenkins/workspace/evmjit/* .'
                  }
                }catch(Exception ex){
                  failRun()
                  throw ex
                }
              }
            }
          }
          stage("Copy etherium tests") {
            steps() {
              script{
                try{
                  sh 'mkdir ethereum_tests'
                  dir('ethereum_tests') {
                    sh 'cp -ar /var/jenkins/workspace/ethereum_tests/* .'
                  }
                }catch(Exception ex){
                  failRun()
                  throw ex
                }
              }
            }
          }
        }
      }

      stage('Write version for GUI') {
        steps() {
          script{
            try{
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
            }catch(Exception ex){
              failRun()
              throw ex
            }
          }
        }
      }

      stage("Configure docker, git, and python") {
        steps {
          script{
            try{
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
                env.release_agent_repo = env.release_repo + "/agent"
                env.release_asset_transfer_repo = env.release_repo + "/asset-transfer"
                env.release_concord_repo = env.release_repo + "/concord-core"
                env.release_ethrpc_repo = env.release_repo + "/ethrpc"
                env.release_fluentd_repo = env.release_repo + "/fluentd"
                env.release_helen_repo = env.release_repo + "/concord-ui"
                env.release_persephone_metadata_repo = env.release_repo + "/persephone-metadata"
                env.release_persephone_provisioning_repo = env.release_repo + "/persephone-provisioning"
                env.release_persephone_fleet_repo = env.release_repo + "/persephone-fleet"
                env.release_ui_repo = env.release_repo + "/ui"
                env.release_contract_compiler_repo = env.release_repo + "/contract-compiler"

                // These are constants which mirror the internal artifactory repos.  We put all merges
                // to master in the internal VMware artifactory.
                env.internal_agent_repo = env.release_agent_repo.replace(env.release_repo, env.internal_repo)
                env.internal_asset_transfer_repo = env.release_asset_transfer_repo.replace(env.release_repo, env.internal_repo)
                env.internal_concord_repo = env.release_concord_repo.replace(env.release_repo, env.internal_repo)
                env.internal_ethrpc_repo = env.release_ethrpc_repo.replace(env.release_repo, env.internal_repo)
                env.internal_fluentd_repo = env.release_fluentd_repo.replace(env.release_repo, env.internal_repo)
                env.internal_helen_repo = env.internal_repo + "/helen"
                env.internal_persephone_metadata_repo = env.release_persephone_metadata_repo.replace(env.release_repo, env.internal_repo)
                env.internal_persephone_provisioning_repo = env.release_persephone_provisioning_repo.replace(env.release_repo, env.internal_repo)
                env.internal_persephone_fleet_repo = env.release_persephone_fleet_repo.replace(env.release_repo, env.internal_repo)
                env.internal_ui_repo = env.release_ui_repo.replace(env.release_repo, env.internal_repo)
                env.internal_contract_compiler_repo = env.release_contract_compiler_repo.replace(env.release_repo, env.internal_repo)
              }

              // Docker-compose picks up values from the .env file in the directory from which
              // docker-compose is run.
              withCredentials([
                string(credentialsId: 'BUILDER_ACCOUNT_PASSWORD', variable: 'PASSWORD'),
                string(credentialsId: 'LINT_API_KEY', variable: 'LINT_API_KEY'),
                string(credentialsId: 'FLUENTD_AUTHORIZATION_BEARER', variable: 'FLUENTD_AUTHORIZATION_BEARER'),
                string(credentialsId: 'VMC_API_TOKEN', variable: 'VMC_API_TOKEN'),
                string(credentialsId: 'DOCKERHUB_REPO_READER_PASSWORD', variable: 'DOCKERHUB_REPO_READER_PASSWORD')
                ]) {
                sh '''
                  echo "${PASSWORD}" | sudo -S ls
                  sudo cat >blockchain/docker/.env <<EOF
agent_repo=${internal_agent_repo}
agent_tag=${docker_tag}
asset_transfer_repo=${internal_asset_transfer_repo}
asset_transfer_tag=${docker_tag}
concord_repo=${internal_concord_repo}
concord_tag=${docker_tag}
ethrpc_repo=${internal_ethrpc_repo}
ethrpc_tag=${docker_tag}
fluentd_repo=${internal_fluentd_repo}
fluentd_tag=${docker_tag}
helen_repo=${internal_helen_repo}
helen_tag=${docker_tag}
persephone_metadata_repo=${internal_persephone_metadata_repo}
persephone_provisioning_repo=${internal_persephone_provisioning_repo}
persephone_fleet_repo=${internal_persephone_fleet_repo}
persephone_tag=${docker_tag}
ui_repo=${internal_ui_repo}
ui_tag=${docker_tag}
contract_compiler_repo=${internal_contract_compiler_repo}
contract_compiler_tag=${docker_tag}
commit_hash=${commit}
LINT_API_KEY=${LINT_API_KEY}
EOF
                  cp blockchain/docker/.env blockchain/hermes/

                  # Need to add the fluentd authorization bearer.
                  # I couldn't get this env method to update the conf https://docs.fluentd.org/v0.12/articles/faq#how-can-i-use-environment-variables-to-configure-parameters-dynamically
                  sed -i -e 's/'"<ADD-LOGINTELLIGENCE-KEY-HERE>"'/'"${FLUENTD_AUTHORIZATION_BEARER}"'/g' blockchain/docker/fluentd/fluentd.conf

                  # Update hermes/resources/persephone/provision-service/config.json for persephone (deployment service) testing
                  sed -i -e 's/'"<VMC_API_TOKEN>"'/'"${VMC_API_TOKEN}"'/g' blockchain/hermes/resources/persephone/provision-service/config.json
                  sed -i -e 's/'"<DOCKERHUB_REPO_READER_PASSWORD>"'/'"${DOCKERHUB_REPO_READER_PASSWORD}"'/g' blockchain/hermes/resources/persephone/provision-service/config.json
                '''
              }

              // Set up python.
              script{
                env.python = "/var/jenkins/workspace/venv_py37/bin/python"
              }
            }catch(Exception ex){
              failRun()
              throw ex
            }
          }
        }
      }

      stage("Build") {
        steps {
          script{
            env.additional_components_to_build = additional_components_to_build
            try{
              dir('blockchain') {
                sh '''
                  ./buildall.sh --additionalBuilds ${additional_components_to_build}
                '''
              }
            }catch(Exception ex){
              failRun()
              throw ex
            }
          }
        }
      }

      stage("Run tests in containers") {
        steps {
          script{
            try{
              dir('blockchain/hermes'){
                withCredentials([string(credentialsId: 'BUILDER_ACCOUNT_PASSWORD', variable: 'PASSWORD')]) {
                  script {
                    env.test_log_root = new File(env.WORKSPACE, "testLogs").toString()
                    env.ui_test_logs = new File(env.test_log_root, "UI").toString()
                    env.asset_transfer_test_logs = new File(env.test_log_root, "AssetTransfer").toString()
                    env.core_vm_test_logs = new File(env.test_log_root, "CoreVM").toString()
                    env.helen_api_test_logs = new File(env.test_log_root, "HelenAPI").toString()
                    env.extended_rpc_test_logs = new File(env.test_log_root, "ExtendedRPC").toString()
                    env.extended_rpc_test_helen_logs = new File(env.test_log_root, "ExtendedRPC-Helen").toString()
                    env.regression_test_logs = new File(env.test_log_root, "Regression").toString()
                    env.statetransfer_test_logs = new File(env.test_log_root, "StateTransfer").toString()
                    env.mem_leak_test_logs = new File(env.test_log_root, "MemoryLeak").toString()
                    env.performance_test_logs = new File(env.test_log_root, "PerformanceTest").toString()
                    env.persephone_test_logs = new File(env.test_log_root, "PersephoneTest").toString()
                    env.lint_test_logs = new File(env.test_log_root, "LintTest").toString()
                    env.contract_compiler_test_logs = new File(env.test_log_root, "ContractCompilerTests").toString()

                    if (genericTests) {
                      sh '''
                        # So test suites not using sudo can write to test_logs.
                        mkdir "${test_log_root}"
                        echo "${PASSWORD}" | sudo -S "${python}" main.py AssetTransferTests --dockerComposeFile ../docker/docker-compose.yml --resultsDir "${asset_transfer_test_logs}" --runConcordConfigurationGeneration
                        echo "${PASSWORD}" | sudo -S "${python}" main.py CoreVMTests --dockerComposeFile ../docker/docker-compose.yml --resultsDir "${core_vm_test_logs}" --runConcordConfigurationGeneration
                        echo "${PASSWORD}" | sudo -S "${python}" main.py HelenAPITests --dockerComposeFile ../docker/docker-compose.yml --resultsDir "${helen_api_test_logs}" --runConcordConfigurationGeneration
                        echo "${PASSWORD}" | sudo -S "${python}" main.py ExtendedRPCTests --dockerComposeFile ../docker/docker-compose.yml --resultsDir "${extended_rpc_test_logs}" --runConcordConfigurationGeneration
                        echo "${PASSWORD}" | sudo -S "${python}" main.py ExtendedRPCTests --dockerComposeFile ../docker/docker-compose.yml --resultsDir "${extended_rpc_test_helen_logs}" --ethrpcApiUrl https://localhost/blockchains/local/api/concord/eth --runConcordConfigurationGeneration
                        echo "${PASSWORD}" | sudo -S "${python}" main.py RegressionTests --dockerComposeFile ../docker/docker-compose.yml --resultsDir "${regression_test_logs}" --runConcordConfigurationGeneration

                        # RV, March 21, 2019: Commenting out this suite because it relies on a native Concord build, which is becoming problematic.
                        #                     Uncomment when it no longer relies on that.
                        # echo "${PASSWORD}" | sudo -S "${python}" main.py SimpleStateTransferTest --dockerComposeFile ../docker/docker-compose.yml --resultsDir "${statetransfer_test_logs}" --runConcordConfigurationGeneration
                        echo "${PASSWORD}" | sudo -S "${python}" main.py TruffleTests --logLevel debug --dockerComposeFile ../docker/docker-compose.yml --resultsDir "${truffle_test_logs}" --runConcordConfigurationGeneration
                        echo "${PASSWORD}" | sudo -S "${python}" main.py ContractCompilerTests --dockerComposeFile ../docker/docker-compose.yml --resultsDir "${contract_compiler_test_logs}" --runConcordConfigurationGeneration

                        cd suites ; echo "${PASSWORD}" | sudo -SE ./memory_leak_test.sh --testSuite CoreVMTests --repeatSuiteRun 2 --tests vmArithmeticTest/add0.json --resultsDir ${mem_leak_test_logs} ; cd ..

                        # We need to delete the database files before running UI tests because
                        # Selenium cannot launch Chrome with sudo.  (The only reason Hermes
                        # needs to be run with sudo is so it can delete any existing DB files.)
                        echo "${PASSWORD}" | sudo -S rm -rf ../docker/devdata/rocksdbdata*
                        echo "${PASSWORD}" | sudo -S rm -rf ../docker/devdata/cockroachDB
                        "${python}" main.py UiTests --dockerComposeFile ../docker/docker-compose.yml --resultsDir "${ui_test_logs}" --runConcordConfigurationGeneration
                      '''
                    }
                    if (env.JOB_NAME.contains(memory_leak_job_name)) {
                      sh '''
                        echo "Running Entire Testsuite: Memory Leak..."
                        cd suites ; echo "${PASSWORD}" | sudo -SE ./memory_leak_test.sh --testSuite CoreVMTests --repeatSuiteRun 5 --resultsDir ${mem_leak_test_logs}
                      '''
                    }
                    if (env.JOB_NAME.contains(performance_test_job_name)) {
                      sh '''
                        echo "Running Entire Testsuite: Performance..."
                        echo "${PASSWORD}" | sudo -SE "${python}" main.py PerformanceTests --dockerComposeFile ../docker/docker-compose.yml --resultsDir "${performance_test_logs}" --runConcordConfigurationGeneration
                      '''
                    }
                    if (env.JOB_NAME.contains(persephone_test_job_name)) {
                      sh '''
                        echo "Running Entire Testsuite: Persephone..."
                        echo "${PASSWORD}" | sudo -SE "${python}" main.py PersephoneTests --dockerComposeFile ../docker/docker-compose-persephone.yml --resultsDir "${persephone_test_logs}"
                      '''
                    }
                    if (env.JOB_NAME.contains(lint_test_job_name)) {
                      sh '''
                        echo "Running Entire Testsuite: Lint E2E..."

                        # We need to delete the database files before running UI tests because
                        # Selenium cannot launch Chrome with sudo.  (The only reason Hermes
                        # needs to be run with sudo is so it can delete any existing DB files.)
                        echo "${PASSWORD}" | sudo -S rm -rf ../docker/devdata/rocksdbdata*
                        echo "${PASSWORD}" | sudo -S rm -rf ../docker/devdata/cockroachDB

                        "${python}" main.py LintTests --dockerComposeFile ../docker/docker-compose.yml ../docker/docker-compose-fluentd.yml --resultsDir "${lint_test_logs}" --runConcordConfigurationGeneration
                      '''
                    }
                  }
                }
              }
            }catch(Exception ex){
              failRun()
              throw ex
            }
          }
        }
      }

      stage ("Post Memory Leak Testrun") {
        when {
          expression { env.JOB_NAME.contains(memory_leak_job_name) }
        }
        stages {
          stage('Push memory leak summary into repo') {
            steps {
              script {
                try {
                  dir('hermes-data/memory_leak_test') {
                    pushHermesDataFile('memory_leak_summary.csv')
                  }
                } catch(Exception ex){
                    failRun()
                    throw ex
                }
              }
            }
          }
          stage ('Send Memory Leak Alert Notification') {
            steps {
              script {
                try {
                  dir('hermes-data/memory_leak_test') {
                    sendAlertNotification('memory_leak')
                  }
                } catch(Exception ex){
                    failRun()
                    throw ex
                }
              }
            }
          }
          stage ('Graph') {
            steps {
              script {
                try {
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
                } catch(Exception ex){
                    failRun()
                    throw ex
                }
              }
            }
          }
        }
      }

      stage ("Post Performance Testrun") {
        when {
          expression { env.JOB_NAME.contains(performance_test_job_name) }
        }
        stages {
          stage('Collect Performance Transaction Rate') {
            steps {
              script {
                try {
                  dir('blockchain/hermes/suites') {
                    withCredentials([string(credentialsId: 'BUILDER_ACCOUNT_PASSWORD', variable: 'PASSWORD')]) {
                      sh '''
                        echo "Collect Transaction Rate from this run..."
                        echo "${PASSWORD}" | sudo -SE ./update_performance_result.sh --resultsDir "${performance_test_logs}"
                      '''
                    }
                  }
                } catch(Exception ex){
                    failRun()
                    throw ex
                }
              }
            }
          }
          stage('Push performance transaction rate into repo') {
            steps {
              script {
                try {
                  dir('hermes-data/performance_test') {
                    pushHermesDataFile('perf_testrun_summary.csv')
                  }
                } catch(Exception ex){
                    failRun()
                    throw ex
                }
              }
            }
          }
          stage ('Send Performance Test Alert Notification') {
            steps {
              dir('hermes-data/performance_test') {
                sendAlertNotification('performance')
              }
            }
          }
          stage ('Graph') {
            steps {
              script {
                try {
                  plot csvFileName: 'plot-summary.csv',
                    csvSeries: [[
                                file: 'perf_testrun_summary.csv',
                                exclusionValues: '',
                                displayTableFlag: false,
                                inclusionFlag: 'OFF',
                                url: '']],
                  group: 'Performance Test Transaction Rate',
                  title: 'Performance Test Transaction Rate',
                  style: 'line',
                  exclZero: false,
                  keepRecords: false,
                  logarithmic: false,
                  numBuilds: '',
                  useDescr: false,
                  yaxis: 'Performance Test Transaction Rate',
                  yaxisMaximum: '',
                  yaxisMinimum: ''
                } catch(Exception ex){
                    failRun()
                    throw ex
                }
              }
            }
          }
        }
      }

      stage("Save to artifactory"){
        when {
          expression {
            env.JOB_NAME.contains(master_branch_job_name)
          }
        }
        steps{
          script {
            try{
              withCredentials([string(credentialsId: 'ARTIFACTORY_API_KEY', variable: 'ARTIFACTORY_API_KEY')]) {
                // Pass in false for whether to tag as latest because VMware's
                // artifactory does not allow re-using a tag.
                pushDockerImage(env.internal_agent_repo, env.docker_tag, false)
                pushDockerImage(env.internal_asset_transfer_repo, env.docker_tag, false)
                pushDockerImage(env.internal_concord_repo, env.docker_tag, false)
                pushDockerImage(env.internal_ethrpc_repo, env.docker_tag, false)
                pushDockerImage(env.internal_fluentd_repo, env.docker_tag, false)
                pushDockerImage(env.internal_helen_repo, env.docker_tag, false)
                pushDockerImage(env.internal_persephone_metadata_repo, env.docker_tag, false)
                pushDockerImage(env.internal_persephone_provisioning_repo, env.docker_tag, false)
                // pushDockerImage(env.internal_persephone_fleet_repo, env.docker_tag, false)
                pushDockerImage(env.internal_ui_repo, env.docker_tag, false)
                pushDockerImage(env.internal_contract_compiler_repo, env.docker_tag, false)
              }
            }catch(Exception ex){
              failRun()
              throw ex
            }
          }
        }
      }

      stage("Release") {
        when {
          environment name: 'deploy', value: 'true'
        }
        steps {
          script{
            try{
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
                  docker tag ${internal_agent_repo}:${docker_tag} ${release_agent_repo}:${docker_tag}
                  docker tag ${internal_asset_transfer_repo}:${docker_tag} ${release_asset_transfer_repo}:${docker_tag}
                  docker tag ${internal_concord_repo}:${docker_tag} ${release_concord_repo}:${docker_tag}
                  docker tag ${internal_ethrpc_repo}:${docker_tag} ${release_ethrpc_repo}:${docker_tag}
                  docker tag ${internal_fluentd_repo}:${docker_tag} ${release_fluentd_repo}:${docker_tag}
                  docker tag ${internal_helen_repo}:${docker_tag} ${release_helen_repo}:${docker_tag}
                  docker tag ${internal_persephone_metadata_repo}:${docker_tag} ${release_persephone_metadata_repo}:${docker_tag}
                  docker tag ${internal_persephone_provisioning_repo}:${docker_tag} ${release_persephone_provisioning_repo}:${docker_tag}
                  # docker tag ${internal_persephone_fleet_repo}:${docker_tag} ${release_persephone_fleet_repo}:${docker_tag}
                  docker tag ${internal_ui_repo}:${docker_tag} ${release_ui_repo}:${docker_tag}
                  docker tag ${internal_contract_compiler_repo}:${docker_tag} ${release_contract_compiler_repo}:${docker_tag}
                '''
                pushDockerImage(env.release_agent_repo, env.docker_tag, true)
                pushDockerImage(env.release_asset_transfer_repo, env.docker_tag, true)
                pushDockerImage(env.release_concord_repo, env.docker_tag, true)
                pushDockerImage(env.release_ethrpc_repo, env.docker_tag, true)
                pushDockerImage(env.release_fluentd_repo, env.docker_tag, true)
                pushDockerImage(env.release_helen_repo, env.docker_tag, true)
                // pushDockerImage(env.release_persephone_metadata_repo, env.docker_tag, true)
                // pushDockerImage(env.release_persephone_provisioning_repo, env.docker_tag, true)
                // pushDockerImage(env.release_persephone_fleet_repo, env.docker_tag, true)
                pushDockerImage(env.release_ui_repo, env.docker_tag, true)
                pushDockerImage(env.release_contract_compiler_repo, env.docker_tag, true)
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
            }catch(Exception ex){
              failRun()
              throw ex
            }
          }
        }
      }

      stage("Success") {
        steps {
          script{
            passRun()
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
// Next, try to get the branch.
// Returns the short form commit hash.
String getRepoCode(repo_url, branch_or_commit){
  refPrefix = "refs/heads/"

  if (branch_or_commit && branch_or_commit.trim()){
    // We don't know if this was a branch or a commit, so don't add the refPrefix.
    checkoutRepo(repo_url, branch_or_commit)
  }else if (env.gitlabSourceBranch && env.gitlabSourceBranch.trim()){
    // When launched via gitlab triggering the pipeline plugin, there is a gitlabSourceBranch
    // environment variable.
    checkoutRepo(repo_url, refPrefix + env.gitlabSourceBranch)
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
  checkout([$class: 'GitSCM', branches: [[name: branch_or_commit]], doGenerateSubmoduleConfigurations: false, extensions: [[$class: 'SubmoduleOption', disableSubmodules: false, parentCredentials: true, recursiveSubmodules: true, reference: '', trackingSubmodules: false]], submoduleCfg: [], userRemoteConfigs: [[credentialsId: 'GITLAB_LDAP_CREDENTIALS', url: repo_url]]])
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

void pushHermesDataFile(fileToPush){
  echo "git add"
  sh (
    script: "git add ${fileToPush}",
    returnStdout: false
  )
  echo "git commit"
  sh (
    script: "git commit -m 'Update summary file'",
    returnStdout: false
  )

  echo "git push"
  sh (
    script: "git push origin master",
    returnStdout: false
  )
}

void sendAlertNotification(test_name) {
  if (test_name == 'memory_leak') {
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

  if (test_name == 'performance') {
    performance_test_spiked_log = new File(env.performance_test_logs, "performance_transaction_rate_spiked.log").toString()
    if (fileExists(performance_test_spiked_log)) {
      echo 'ALERT: Performance Transaction Rate spiked up in this run'

      performance_test_alert_notification_address_file = "performance_test_alert_recipients.txt"
      if (fileExists(performance_test_alert_notification_address_file)) {
        performance_test_alert_notification_recipients = readFile(performance_test_alert_notification_address_file).replaceAll("\n", " ")
        echo 'Sending ALERT email notification...'
        emailext body: "Performance Transaction Rate Spiked up in build: ${env.BUILD_NUMBER}\n\n More info at: ${env.BUILD_URL}\nPerformance Result Log file: ${env.BUILD_URL}artifact/testLogs/PerformanceTest/performance_result.log\n\nGraph: ${JOB_URL}plot",
        to: performance_test_alert_notification_recipients,
        subject: "ALERT: Performance Transaction Rate Spiked up in build ${env.BUILD_NUMBER}"
      }
    }
  }
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

// Called when it begins.
// Don't call for individual stages.
void startRun(){
  updateGitlabCommitStatus(name: "Jenkins Run", state: "running")
}

// Called when it is successful.
void passRun(){
  updateGitlabCommitStatus(name: "Jenkins Run", state: "success")
}

// Called when it fails.
void failRun(){
  updateGitlabCommitStatus(name: "Jenkins Run", state: "failed")
}

// Given a host to connect to, use ssh-keygen to see if we have
// its ssh key in known_hosts. If not, use ssh-keyscan to fetch it,
// then verify with ssh-keygen.
void handleKnownHosts(host){
  // By setting returnStatus, we will get an exit code instead of
  // having the entire Jenkins run fail.
  status = sh (
    script: "ssh-keygen -F " + host,
    returnStatus: true
  )

  if(status != 0){
    // ssh-keyscan throws a nice error; let it bubble up.
    sh (
      script: "ssh-keyscan -H " + host + " >> ~/.ssh/known_hosts",
    )

    status = sh (
      script: "ssh-keygen -F " + host,
      returnStatus: true
    )

    if(status != 0){
      error("Unable to retrieve the ssh key for " + host)
    }
  }
}
