import groovy.json.JsonOutput
import groovy.json.JsonSlurperClassic
import groovy.transform.Field
import hudson.util.Secret

// Notes about this map:
// - Possible fields per suite:
//   "enabled": Boolean. If false, the test absolutely cannot be run, no matter
//     what the user selects.
//   "baseCommand": String, base shell command, defaults to "echo <pwd> | sudo -SE <python> main.py <suite>".
//     This is how you can use command line parameters which are not covered by other
//     entries.
//   "dockerComposeFiles": String, docker compose files to use.
//   "runConcordConfigurationGeneration": Whether to add
//     "--runConcordConfigurationGeneration" to the command, default is true
//   "resultsDir": String, defaults to the map key
//   "performanceVotes": Int, for performance testing
//   "concordConfigurationInput": String, files to use for concord config generation.
//   "runAlone": Boolean, causes this suite to be run on a clean invocation of the product, by itself.
//   "runWithGenericTests": Boolean, whether to run in a job not named in specialized_tests. Defaults to true.
//   "runWithToTTests": Boolean, whether to run with ToT runs.  Defaults to true.
//   "suiteDir": String, directory to cd into (and then out of when done)
//   "setupFunction": String, name of a function in this file to run before running the suite
// - Keys have to match the Jenkins job's list of test suites. We can't read them
//   from the same place, so be careful.
// - Do not declare it with "def". Due to the way Jenkins uses this Groovy code,
//   that will cause it to not be in scope.  We have to use @Field.
// - Jenkins does not allow iterating through a map's entries because the map
//   iterator is not serializable, and Jenkins serializes things so it can
//   resume runs.  We can, however, create a list of keys and iterate over
//   that list.
// - Every suite has a default value for something because [] evaluates to a
//   list, while ["foo": "bar"] evaluates to a map, and we want to use the map
//   api.
// - Move this to another file?
@Field Map testSuites = [
  "ApolloBftTests": [
    "runAlone": true,
    "enabled": true,
    "dockerComposeFiles": "../docker/docker-compose-tee.yml",
    "concordConfigurationInput": "/concord/config/dockerConfigurationInput-tee.yaml"
  ],
  "SampleSuite": [
    "enabled": true
  ],
  "SampleDAppTests": [
    "enabled": true
  ],
  "EthCoreVmTests": [
    "enabled": true
  ],
  "PerformanceTests": [
    "enabled": true,
    "concordConfigurationInput": "/concord/config/dockerConfigurationInput-perftest.yaml",
    "performanceVotes": 10
  ],
  "HelenAPITests": [
    "enabled": true
  ],
  "HelenRoleTests": [
    "enabled": true,
    "runWithGenericTests": false,
    "runWithToTTests": false
  ],
  "EthJsonRpcTests": [
    "enabled": true
  ],
  "EthJsonRpcTestsEthrpc": [
    "enabled": true,
    "baseCommand": 'echo "${PASSWORD}" | sudo -S "${python}" main.py EthJsonRpcTests --ethrpcApiUrl https://localhost/blockchains/local/api/concord/eth'
  ],
  "RegressionTests": [
    "enabled": true,
    "runAlone": true
  ],
  "DamlTests": [
    "enabled": true,
    "dockerComposeFiles": "../docker/docker-compose-daml.yml",
    "concordConfigurationInput": "/concord/config/dockerConfigurationInput-daml.yaml"
  ],
  "ThinReplicaServerTests": [
    "enabled": true,
    "dockerComposeFiles": "../docker/docker-compose-daml-nano.yml",
    "concordConfigurationInput": "/concord/config/dockerConfigurationInput-daml-nano.yaml"
  ],
  "PrivacyTeeTests": [
    "enabled": false,
    "dockerComposeFiles": "../docker/docker-compose-tee.yml",
    "concordConfigurationInput": "/concord/config/dockerConfigurationInput-tee.yaml"
  ],
  "SimpleStateTransferTest": [
    "enabled": true,
    "dockerComposeFiles": "../docker/docker-compose.yml ../docker/docker-compose-static-ips.yml"
  ],
  "ContractCompilerTests": [
    "enabled": true
  ],
  "MetadataPersistencyTests": [
    "enabled": false,
    "dockerComposeFiles": "../docker/docker-compose.yml ../docker/docker-compose-static-ips.yml",
    "baseCommand": 'echo "${PASSWORD}" | sudo -S "${python}" main.py MetadataPersistencyTests --ethrpcApiUrl https://localhost:8547/blockchains/local/api/concord/eth'
  ],
  "HlfTests": [
    "enabled": false, // RV: Disabled because these repeatedly cause the product to fail to launch in CI/CD.
    "concordConfigurationInput": "/concord/config/dockerConfigurationInput-hlf.yaml"
  ],
  "TimeTests": [
    "enabled": true,
    "concordConfigurationInput": "/concord/config/dockerConfigurationInput-time_service.yaml",
    "setupFunction": "enableTimeService"
  ],
  "EvilTimeTests": [
    "enabled": true,
    "concordConfigurationInput": "/concord/config/dockerConfigurationInput-time_service.yaml",
    "setupFunction": "enableTimeService"
  ],
  "MemoryLeakTests": [
    "enabled": true,
    "suiteDir": "suites",
    "baseCommand": 'echo "${PASSWORD}" | sudo -SE ./memory_leak_test.sh --testSuite EthCoreVmTests \
      --repeatSuiteRun 2 --tests \'vmArithmeticTest/add0.json\''
  ],
  "UiTests": [
    "enabled": true,
    "setupFunction": "deleteDatabaseFiles",
    "dockerComposeFiles": "../docker/docker-compose.yml ../docker/docker-compose-persephone.yml",
    "baseCommand": '"${python}" main.py UiTests'
  ],
  "LoggingTests": [
    "enabled": true,
    "dockerComposeFiles": "../docker/docker-compose.yml ../docker/docker-compose-persephone.yml",
    "runWithGenericTests": false,
    "runWithToTTests": false,
    "baseCommand": 'echo "${PASSWORD}" | sudo -S "${python}" main.py LoggingTests --blockchainType daml --numReplicas 7 --blockchainLocation sddc'
  ],
  "UiDAMLDeploy": [
    "enabled": false,
    "runWithGenericTests": false,
    "runWithToTTests": false,
    "setupFunction": "deleteDatabaseFiles",
    "dockerComposeFiles": "../docker/docker-compose.yml ../docker/docker-compose-persephone.yml",
    "baseCommand": '"${python}" main.py DeployDamlTests'
  ],
  "HelenDeployEthereumToSDDC": [
    "enabled": true,
    "dockerComposeFiles": "../docker/docker-compose.yml ../docker/docker-compose-persephone.yml",
    "baseCommand": 'echo "${PASSWORD}" | sudo -S "${python}" main.py EthCoreVmTests --blockchainLocation sddc \
      --tests="-k vmArithmeticTest/add0.json" --suitesRealname=HelenDeployEthereumToSDDC'
  ],
  "HelenDeployDAMLToSDDC" : [
    "enabled": false,
    "dockerComposeFiles": "../docker/docker-compose.yml ../docker/docker-compose-persephone.yml",
    "baseCommand": 'echo "${PASSWORD}" | sudo -S "${python}" main.py HelenAPITests --blockchainType daml  \
      --blockchainLocation onprem --numReplicas 7 --numParticipants 1 \
      --tests="-m deployment_only" --suitesRealname=HelenDeployDAMLToSDDC'
  ],
  "HelenDeployToSDDCTemplate" : [
    "enabled": false,
    "dockerComposeFiles": "../docker/docker-compose.yml ../docker/docker-compose-persephone.yml",
    "baseCommand": 'echo "${PASSWORD}" | sudo -S "${python}" main.py HelenAPITests --test="-m deployment_only" \
      --suitesRealname=HelenDeployToSDDCTemplate'
  ]
]

// Pipline env
@Field String agentLabel = "genericVM"
@Field Boolean genericTests = true
@Field String additional_components_to_build = ""

// Job Names List
@Field String deploy_concord_job_name = "Concord Deployment Test"
@Field String deployment_support_bundle_job_name = "Get Deployment support bundle"
@Field String ext_long_tests_job_name = "Blockchain Extensive Long Tests"
@Field String helen_role_test_job_name = "Helen Role Tests on GitLab"
@Field String log_insight_test_job_name = "Log Insight Integration Test"
@Field String long_tests_job_name = "Blockchain Long Tests"
@Field String main_mr_run_job_name = "Main Blockchain Run on GitLab"
@Field String memory_leak_job_name = "BlockchainMemoryLeakTesting"
@Field String monitor_replicas_job_name = "Monitor Blockchain replica health and status"
@Field String performance_test_job_name = "Blockchain Performance Test"
@Field String persephone_test_job_name = "Blockchain Persephone Tests"
@Field String persephone_test_on_demand_job_name = "ON DEMAND Persephone Testrun on GitLab"
@Field String ui_e2e_daml_on_prem_job_name = "UI E2E Deploy DAML On Premises"
@Field String manual_with_params_job_name = "Blockchain Manual Run"

// These runs will never run Persehpone tests. Persephone tests have special criteria,
// and these runs can end up running them unintentionally.
@Field List runs_excluding_persephone_tests = [
  deploy_concord_job_name,
  deployment_support_bundle_job_name,
  ext_long_tests_job_name,
  helen_role_test_job_name,
  log_insight_test_job_name,
  long_tests_job_name,
  memory_leak_job_name,
  monitor_replicas_job_name,
  performance_test_job_name,
  ui_e2e_daml_on_prem_job_name,
]

// These job names are just substrings of the actual job names.
@Field List specialized_tests = [
  deploy_concord_job_name,
  deployment_support_bundle_job_name,
  ext_long_tests_job_name,
  helen_role_test_job_name,
  log_insight_test_job_name,
  long_tests_job_name,
  memory_leak_job_name,
  monitor_replicas_job_name,
  performance_test_job_name,
  persephone_test_job_name,
  persephone_test_on_demand_job_name,
  ui_e2e_daml_on_prem_job_name,
]



def call(){
  // This is a unique substring of the Jenkins job which tests ToT after a
  // change has been merged.
  // Change this when creating a release branch.
  env.tot_job_name = "Master Branch"

  // This is the name of the branch into which a commit is being merged,
  // and the name of the branch whose ToT will be used for the above
  // Jenkins run (tot_job_name).
  // It would be great if we could get this from the shared lib load branch
  // instead of hard coding, but I don't see it in the env.
  // Change this when creating a release branch.
  env.tot_branch = "master"

  for (specialized_test in specialized_tests){
    if (env.JOB_NAME.contains(specialized_test)){
      echo "**** Jenkins job for " + env.JOB_NAME
      genericTests = false
      break
    }
  }

  if (genericTests){
    echo "**** Jenkins job for Generic Test Run"
  }

  if (env.JOB_NAME.contains(ext_long_tests_job_name)) {
    agentLabel = "ExtLongRunTest"
  } else if (env.JOB_NAME.contains(long_tests_job_name)) {
    agentLabel = "LongRunTest"
  } else if (env.JOB_NAME.contains(memory_leak_job_name)) {
    agentLabel = "MemoryLeakTesting"
  }

  pipeline {
    agent { label params.jenkins_node ? params.jenkins_node : agentLabel }
    tools {
      nodejs 'Node 11.15.0'
    }
    options{
      gitLabConnection('TheGitlabConnection')
    }
    parameters {
      booleanParam defaultValue: false, description: "Whether to deploy the docker images for production. Only supported for the ToT Jenkins jobs", name: "deploy"

      booleanParam defaultValue: true, description: "Whether to run tests.", name: "run_tests"
      password defaultValue: "",
             description: "If this is a run which pushes items to a repo (e.g. Artifactory, Bintray, Dockerhub), and you are bypassing tests, a password is needed.",
             name: "skip_tests_password"

      string defaultValue: "",
             description: "Blockchain commit or branch to use.  Providing a branch name will pull the branch's latest commit.",
             name: "blockchain_branch_or_commit"
      booleanParam defaultValue: true,
                   description: "Whether to merge the above commit/branch into the local master or release branch before building and testing.\
  This is a --no-ff merge, just like GitLab will do when it will try to merge your change.",
                   name: "merge_branch_or_commit"

      string defaultValue: "",
             description: "Shared Jenkins lib branch to use.",
             name: "shared_lib_branch"

      string defaultValue: "",
             description: "Override automatic node selection and run this job on a node with this label.",
             name: "jenkins_node"

      string defaultValue: "LastSuccessfulToT",
             description: "Concord deployment: Specify concord tag for the deployment [LastSuccessfulToT/ThisBranch/<Specific tag like 0.0.0.1246>].",
             name: "concord_deployment_tag"

      choice(choices: "on-failure\nnever\nalways", description: 'Persephone Tests: Choose a deployment test failure Retention Policy', name: 'deployment_retention')

      string defaultValue: "10",
             description: "Performance Test: Enter number of votes for Ballot App (default 10 votes)",
             name: "performance_votes"

      string defaultValue: "",
             description: "To collect deployment support bundle, enter comma separated list of concord node IPs",
             name: "concord_ips"
      choice(choices: "DAML\nETHEREUM", description: 'To collect deployment support bundle, choose a concord type', name: 'concord_type')

      string defaultValue: "",
             description: "Monitor replicas: Enter a repeated set of blockchain_type:<set of replicas> (example: daml_committer:10.70.30.226,10.70.30.225,10.70.30.227,10.70.30.228 daml_participant:10.70.30.229)",
             name: "replicas_with_bc_type"
      choice(choices: "SDDC\nONPREM", description: 'Monitor replicas: Choose blockchain location', name: 'blockchain_location')

      string defaultValue: "72",
             description: "Monitor replicas: Enter number of hours to monitor replicas (default 72 hrs)",
             name: "run_duration"
      string defaultValue: "60",
             description: "Monitor replicas: Enter number of minutes to wait between monitors (default 60 mins)",
             name: "load_interval"
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
              env.eventsFile = "times.json"
              env.eventsFullPath = env.WORKSPACE + "/" + env.eventsFile
              env.eventsRecorder = env.WORKSPACE + "/blockchain/hermes/event_recorder.py"
              checkSkipTestsPassword()
              removeContainers()
              pruneImages()
              reportSystemStats()
              printSelectableSuites()

              // Set as env variables
              env.concord_deployment_tag = params.concord_deployment_tag
              env.deployment_retention = params.deployment_retention
              env.performance_votes = params.performance_votes
              env.concord_ips = params.concord_ips
              env.concord_type = params.concord_type
              env.replicas_with_bc_type = params.replicas_with_bc_type
              env.blockchain_location = params.blockchain_location
              env.run_duration = params.run_duration
              env.load_interval = params.load_interval

              // Check parameters
              errString = "Parameter check error: "

              if (params.deploy && (!env.JOB_NAME.contains(env.tot_job_name))){
                throw new Exception (errString + "For this branch, only do releases from the '" +
                      env.tot_job_name + "' Jenkins job.  The build number is based on that.")
              }

              // chown files in case something was left behind owned by root, so we can cleanWs().
              withCredentials([string(credentialsId: 'BUILDER_ACCOUNT_PASSWORD', variable: 'PASSWORD')]) {
                sh '''
                  echo "${PASSWORD}" | sudo -S chown -R builder:builder .
                '''
              }

              cleanWs()

              // Add the VMware GitLab ssh key to known_hosts.
              handleKnownHosts("gitlab.eng.vmware.com")

              // Try dealing with https://issues.jenkins-ci.org/browse/JENKINS-48300. Run failed with this error:
              // "JENKINS-48300: if on an extremely laggy filesystem, consider -Dorg.jenkinsci.plugins.durabletask.BourneShellScript.HEARTBEAT_CHECK_INTERVAL=86400"
              System.setProperty("org.jenkinsci.plugins.durabletask.BourneShellScript.HEARTBEAT_CHECK_INTERVAL", "86400")

            }catch(Exception ex){
              failRun()
              throw ex
            }
          }
        }
      }

      stage("Fetch blockchain repo source") {
        steps {
          script{
            try{
              script {
                fetchSourceRepos()
                env.blockchain_root = new File(env.WORKSPACE, "blockchain").toString()
                // Check if persephone tests are to be executed in this run
                env.run_persephone_tests = need_persephone_tests(runs_excluding_persephone_tests)
                echo "Run Persephone Tests? " + env.run_persephone_tests
              }
            }catch(Exception ex){
              failRun()
              throw ex
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
                  saveTimeEvent("Setup", "Copy googletest")
                  sh 'mkdir googletest'
                  dir('googletest') {
                    sh 'cp -ar /var/jenkins/workspace/googletest/* .'
                  }
                  saveTimeEvent("Setup", "Finished copying googletest")
                }catch(Exception ex){
                  failRun()
                  throw ex
                }
              }
            }
          }
          stage("Add localhost.vmware.com") {
            steps {
              dir('blockchain/vars') {
                withCredentials([string(credentialsId: 'BUILDER_ACCOUNT_PASSWORD', variable: 'PASSWORD')]) {
                  sh 'echo "${PASSWORD}" | sudo -S ./add-localhost-vmware-com.sh'
                }
              }
            }
          }
          stage("Copy evmjit") {
            steps() {
              script{
                try{
                  saveTimeEvent("Setup", "Copy evmjit")
                  sh 'mkdir evmjit'
                  dir('evmjit') {
                    sh 'cp -ar /var/jenkins/workspace/evmjit/* .'
                  }
                  saveTimeEvent("Setup", "Finished copying evmjit")
                }catch(Exception ex){
                  failRun()
                  throw ex
                }
              }
            }
          }
          stage("Copy ethereum tests") {
            steps() {
              script{
                try{
                  saveTimeEvent("Setup", "Copy ethereum tests")
                  sh 'mkdir ethereum_tests'
                  dir('ethereum_tests') {
                    sh 'cp -ar /var/jenkins/workspace/ethereum_tests/* .'
                  }
                  saveTimeEvent("Setup", "Finished copying ethereum tests")
                }catch(Exception ex){
                  failRun()
                  throw ex
                }
              }
            }
          }
        }
      }

      stage('Get Version') {
        steps() {
          script{
            try{
              dir('blockchain') {
                script {
                  setProductVersion()
                  echo("product_version: " + env.product_version)
                  env.version_json = createGUIVersionInfo()
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

              withCredentials([string(credentialsId: 'BLOCKCHAIN_REPOSITORY_WRITER_PWD', variable: 'DOCKERHUB_PASSWORD')]) {
                script{
                  command = "docker login -u blockchainrepositorywriter -p " + env.DOCKERHUB_PASSWORD
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

              if (env.JOB_NAME.contains(main_mr_run_job_name)) {
                preprocessForMainMR()
              }

              if (env.JOB_NAME.contains(persephone_test_job_name)) {
                def latest_docker_tag = updateOneCloudProvisioningBintrayAndGetLatestTag()
                setDockerTag(latest_docker_tag)
              } else if(
                  env.JOB_NAME.contains(deploy_concord_job_name) ||
                  env.JOB_NAME.contains(ext_long_tests_job_name) ||
                  env.JOB_NAME.contains(long_tests_job_name) ||
                  env.JOB_NAME.contains(memory_leak_job_name) ||
                  env.JOB_NAME.contains(monitor_replicas_job_name) ||
                  env.JOB_NAME.contains(performance_test_job_name) ||
                  env.JOB_NAME.contains(persephone_test_on_demand_job_name)
                ) {

                dir('blockchain'){
                  def latest_docker_tag = artifactorylib.getLatestTag()
                }
                setDockerTag(latest_docker_tag)
              } else {
                echo "This run requires building components"
                env.docker_tag = env.product_version
              }

              setUpRepoVariables()
              setEnvFileAndUserConfig()

              dir('blockchain'){
                pythonlib.initializePython()
              }

              racetrackSet("begin")

            }catch(Exception ex){
              println("Unable to set up environment: ${ex}")
              failRun()
              throw ex
            }
          }
        }
      }


      stage("Build") {
        steps {
          archiveArtifacts artifacts: env.eventsFile, allowEmptyArchive: false
          script{
            env.additional_components_to_build = additional_components_to_build
            try{
              saveTimeEvent("Build", "Start buildall.sh")
              dir('blockchain') {
                if (env.JOB_NAME.contains(deploy_concord_job_name)) {
                  if (env.concord_deployment_tag.toLowerCase() == "thisbranch") {
                    sh '''
                      echo "Building concord..."
                      ./buildall.sh --buildOnDemand concord
                    '''
                  }
                  sh '''
                    echo "Building grpc binding..."
                    ./buildall.sh --buildOnDemand BuildPersephoneGRPCpyBindings
                  '''
                } else if (env.JOB_NAME.contains(persephone_test_on_demand_job_name)) {
                  sh '''
                    echo "Building ONLY persephone related components..."
                    ./buildall.sh --buildOnDemand concord,waitForProcesses,persephone,BuildPersephoneGRPCpyBindings
                  '''
                } else if (env.JOB_NAME.contains(persephone_test_job_name)) {
                  sh '''
                    echo "For nightly run, building ONLY persephone GRPC bindings..."
                    ./buildall.sh --buildOnDemand BuildPersephoneGRPCpyBindings
                  '''
                } else if (env.JOB_NAME.contains(performance_test_job_name)) {
                  sh '''
                    echo "For Performance nightly run, building ONLY performance & benchmark tool..."
                    ./buildall.sh --buildOnDemand PerformanceTests
                  '''
                } else if (env.JOB_NAME.contains(memory_leak_job_name)) {
                  sh '''
                    echo "No local build required for Memory Leak Testrun..."
                  '''
                } else if (env.JOB_NAME.contains(monitor_replicas_job_name)) {
                  sh '''
                    echo "No local build required for monitoring health and status of replicas..."
                  '''
                } else if (env.JOB_NAME.contains(deployment_support_bundle_job_name)) {
                  sh '''
                    echo "No local build required for collecting deployment support bundle..."
                  '''
                } else if (env.JOB_NAME.contains(long_tests_job_name)) {
                  sh '''
                    echo "No local build required for Blockchain Long Tests..."
                  '''
                } else {
                  sh '''
                    echo "Building All components..."
                    ./buildall.sh --additionalBuilds ${additional_components_to_build}
                  '''
                }
              }
              saveTimeEvent("Build", "Finished buildall.sh")
            }catch(Exception ex){
              failRun()
              throw ex
            }
          }
          archiveArtifacts artifacts: env.eventsFile, allowEmptyArchive: false
        }
      }

      stage("Start tests"){
        steps {
          script {
            saveTimeEvent("Tests", "Start")
          }
        }
      }

      stage("Push Concord components to DockerHub"){
        when {
          expression {
            // Skip this step for nightly or other random runs.
            // Nightly runs use what is in artifactory, which has already been pushed
            // to GitHub.
            env.JOB_NAME.contains(env.tot_job_name) ||
            env.JOB_NAME.contains(main_mr_run_job_name) ||
            env.JOB_NAME.contains(manual_with_params_job_name)
          }
        }
        steps{
          script {
            try{
              saveTimeEvent("Push Concord components to DockerHub", "Start")
              pushConcordComponentsToDockerHub()
              saveTimeEvent("Push Concord components to DockerHub", "End")
            }catch(Exception ex){
              failRun()
              throw ex
            }
          }
        }
      }

      stage("Run tests in containers") {
        when {
          expression {
            params.run_tests
          }
        }
        steps {
          script{
            try{
              dir('blockchain/hermes'){
                withCredentials([string(credentialsId: 'BUILDER_ACCOUNT_PASSWORD', variable: 'PASSWORD')]) {
                  script {
                    // AAAAARGH mixed camel/snake.  We must fix this file.
                    env.test_log_root = new File(env.WORKSPACE, "testLogs").toString()
                    env.concord_deployment_test_logs = new File(env.test_log_root, "ConcordDeploymentTest").toString()
                    env.deployment_support_logs = new File(env.test_log_root, "DeploymentSupportBundle").toString()
                    env.monitor_replicas_logs = new File(env.test_log_root, "MonitorReplicas").toString()
                    env.mem_leak_test_logs = new File(env.test_log_root, "MemoryLeak").toString()
                    env.performance_test_logs = new File(env.test_log_root, "PerformanceTest").toString()
                    env.persephone_test_logs = new File(env.test_log_root, "PersephoneTest").toString()
                    env.lint_test_logs = new File(env.test_log_root, "LintTest").toString()
                    env.log_insight_logs = new File(env.test_log_root, "LogInsightTest").toString()

                    if (genericTests) {
                      if (isGitLabRun()){
                        // Keep running everything.  We're going to allow selection for GitLab runs later.
                        echo("Running all enabled generic tests")
                        for (suite in testSuites.keySet()){
                          if (testSuites[suite].enabled &&
                              (testSuites[suite].runWithGenericTests || testSuites[suite].runWithGenericTests == null)){
                            testSuites[suite].runSuite = true
                          }
                        }
                      } else if (env.JOB_NAME.contains(env.tot_job_name)){
                        echo("Running all enabled tests for a ToT run")
                        for (suite in testSuites.keySet()){
                          echo("Suite: " + suite + " runWithToTTests: " + testSuites[suite].runWithToTTests)
                          if (testSuites[suite].enabled &&
                              (testSuites[suite].runWithToTTests || testSuites[suite].runWithToTTests == null)){
                            testSuites[suite].runSuite = true
                          }
                        }
                      } else {
                        // This was something like manual build with parameters.  Gather tests from the
                        // user's choices.
                        choices = null

                        if (params.tests_to_run == null) {
                          // This shouldn't happen in our workflows, but it could be possible as more
                          // Jenkins jobs are created.
                          echo("tests_to_run was null, so all selectable suites will be selected.")
                          choices = getSelectableSuites()
                        } else {
                          echo("tests_to_run: " + params.tests_to_run)
                          echo("Using suites selected by the user in tests_to_run.")
                          choices = params.tests_to_run.split(",")
                        }

                        echo("Test suite choices: " + choices)

                        for (suite in testSuites.keySet()){
                          if (testSuites[suite].enabled){
                            testSuites[suite].runSuite = choices.contains(suite)
                            echo("Will run suite " + suite + ": " + testSuites[suite].runSuite)
                          }else{
                            echo("Test suite " + suite + " is disabled and will not be run, even if selected.")
                            testSuites[suite].runSuite = false
                          }
                        }
                      }
                      runTests()
                    } else if (env.JOB_NAME.contains(helen_role_test_job_name)) {
                      selectOnlySuites(["HelenRoleTests"])
                      runTests()
                    } else if (env.JOB_NAME.contains(log_insight_test_job_name)) {
                      selectOnlySuites(["LoggingTests"])
                      runTests()
                    } else if (env.JOB_NAME.contains(ui_e2e_daml_on_prem_job_name)) {
                      selectOnlySuites(["UiDAMLDeploy", "UiTests"])
                      runTests()
                    } else if (env.JOB_NAME.contains(long_tests_job_name)) {
                      try {
                        env.blockchain_location = "sddc"
                        testSuites["HelenDeployToSDDCTemplate"]["otherParameters"] =
                            " --blockchainType " + params.concord_type.toLowerCase() +
                            " --blockchainLocation " + env.blockchain_location +
                            " --numReplicas " + params.num_replicas + " "
                        selectOnlySuites(["HelenDeployToSDDCTemplate"])
                        runTests()
                      } catch(Exception ex) {
                        env.fixture_setup_message = "<RUN> has failed to deploy blockchain fixture to SDDC.\n" + env.BUILD_URL + "console"
                        sh '''
                         "${python}" invoke.py slackReportMonitoring --param "${fixture_setup_message}"
                        '''
                        throw ex
                      }

                      sh '''
                        "${python}" invoke.py lrtPrintDashboardLink
                        echo "Running script to monitor health and status of replicas..."
                        cd ../docker ; docker-compose -f ../docker/docker-compose-persephone.yml up -d ; cd -
                        "${python}" monitor_replicas.py --replicasConfig /tmp/replicas.json --loadInterval "${load_interval}" --runDuration "${run_duration}" --saveSupportLogsTo "${monitor_replicas_logs}" --blockchainLocation "${blockchain_location}"
                      '''
                    }

                    // TODO: Make the items below follow the model above.

                    if (env.JOB_NAME.contains(deployment_support_bundle_job_name)) {
                      saveTimeEvent("Collect deployment support bundle", "Start")
                      sh '''
                        echo "Running script to collect deployment support bundle..."
                        "${python}" create_deployment_support.py --replicas "${concord_ips}" --replicaType "${concord_type}" --saveTo "${deployment_support_logs}"
                      '''
                      saveTimeEvent("Collect deployment support bundle", "End")
                    }
                    if (env.JOB_NAME.contains(monitor_replicas_job_name)) {
                      saveTimeEvent("Monitor health and status of replicas", "Start")
                      py_arg_replica_with_bc_type = ""
                      for (replica_set in env.replicas_with_bc_type.split()) {
                        py_arg_replica_with_bc_type = py_arg_replica_with_bc_type + " --replicas " + replica_set
                      }
                      env.py_arg_replica_with_bc_type = py_arg_replica_with_bc_type
                      sh '''
                        echo "Running script to monitor health and status of replicas..."
                        cd ../docker ; docker-compose -f ../docker/docker-compose-persephone.yml up -d ; cd -
                        "${python}" monitor_replicas.py ${py_arg_replica_with_bc_type} --runDuration "${run_duration}" --loadInterval "${load_interval}" --saveSupportLogsTo "${monitor_replicas_logs}" --blockchainLocation "${blockchain_location}"
                      '''
                      saveTimeEvent("Monitor health and status of replicas", "End")
                    }
                    if (env.JOB_NAME.contains(memory_leak_job_name)) {
                      saveTimeEvent("Memory leak tests", "Start")
                      sh '''
                        echo "Running Entire Testsuite: Memory Leak..."
                        cd suites ; echo "${PASSWORD}" | sudo -SE ./memory_leak_test.sh --testSuite EthCoreVmTests --repeatSuiteRun 6 --resultsDir "${mem_leak_test_logs}"
                      '''
                      saveTimeEvent("Memory leak tests", "End")
                    }
                    if (env.JOB_NAME.contains(performance_test_job_name)) {
                      saveTimeEvent("Performance tests", "Start")
                      sh '''
                        echo "Running Entire Testsuite: Performance..."
                        mkdir -p "${performance_test_logs}"
                        echo "${PASSWORD}" | sudo -SE "${python}" main.py PerformanceTests --dockerComposeFile ../docker/docker-compose.yml --performanceVotes 10000 --resultsDir "${performance_test_logs}" --runConcordConfigurationGeneration --concordConfigurationInput /concord/config/dockerConfigurationInput-perftest.yaml --logLevel debug > "${performance_test_logs}/performance_tests.log" 2>&1
                      '''
                      saveTimeEvent("Performance tests", "End")
                    }
                    if (env.JOB_NAME.contains(deploy_concord_job_name)) {
                      saveTimeEvent("Concord deployment test", "Start")
                      if (env.concord_deployment_tag.toLowerCase() == "thisbranch") {
                        env.dep_comp_concord_tag = env.product_version
                        tagAndPushDockerImage(env.internal_concord_repo, env.release_concord_repo, env.dep_comp_concord_tag)
                      }
                      sh '''
                        echo "Running Persephone deployment for concord..."
                        mkdir -p "${concord_deployment_test_logs}"
                        echo "**** Using concord tag: " + ${dep_comp_concord_tag}
                        echo "${PASSWORD}" | sudo -SE "${python}" main.py PersephoneTests --dockerComposeFile ../docker/docker-compose-persephone.yml --resultsDir "${concord_deployment_test_logs}" --keepBlockchains ${deployment_retention} > "${concord_deployment_test_logs}/concord_deployment_test.log" 2>&1
                      '''
                      saveTimeEvent("Concord deployment test", "End")
                    }

                  }
                }
              }
            }catch(Exception ex){
              echo("A failure occurred while running the tests.")

              // See if this suite failed due to SR 19062354609.
              // HelenDeployEthereumToSDDC is the only one of the above which uses Persephone.
              helen_deploy_test_log_dir = new File(env.test_log_root, "HelenDeployEthereumToSDDC").toString()
              handleFailedTestSuite(helen_deploy_test_log_dir)

              failRun()
              throw ex
            }
          }
        }
      }

      stage("Push Images required for Deployment Services & run tests") {
        when {
          expression {
            params.run_tests && (env.run_persephone_tests == "true")
          }
        }
        steps {
          script{
            try{
              saveTimeEvent("Persephone tests", "Start")

              dir('blockchain/hermes') {
                withCredentials([
                  string(credentialsId: 'BUILDER_ACCOUNT_PASSWORD', variable: 'PASSWORD'),
                ]) {
                  script {
                    // For nightly run, deployment components are recent builds already published to bintray
                    if (env.JOB_NAME.contains(persephone_test_job_name)) {
                      echo "For Persephone nightly run, push recent builds to bintray..."

                      saveTimeEvent("Build", "Push recent builds to bintray using Job Setup-SAAS-Artifacts")
                      build job: 'Setup-SAAS-Artifacts',
                                 parameters: [[$class: 'StringParameterValue',
                                                name: 'INTERNALTAG',
                                                value: env.recent_published_docker_tag],
                                              [$class: 'StringParameterValue',
                                                name: 'EXTERNALTAG',
                                                value: env.recent_published_docker_tag]
                                             ]
                      saveTimeEvent("Build", "Completed Push recent builds to bintray using Job Setup-SAAS-Artifacts")

                      env.dep_comp_docker_tag = env.recent_published_docker_tag
                    } else {
                      // For everything else, use what we pushed to DockerHub earlier.
                      env.dep_comp_docker_tag = env.docker_tag
                    }

                    sh(script: 'mkdir -p "${persephone_test_logs}"')

                    // Kashfat: Revert back.
                    if (env.JOB_NAME.contains(persephone_test_job_name)) {
                      sh '''
                        echo "Running Entire Testsuite: Persephone..."
                        echo "${PASSWORD}" | sudo -SE "${python}" main.py PersephoneTests --useLocalConfigService --externalProvisioningServiceEndpoint ${EXT_PROVISIONING_SERVICE_ENDPOINT} --dockerComposeFile ../docker/docker-compose-persephone.yml --tests "all_tests" --resultsDir "${persephone_test_logs}" --keepBlockchains ${deployment_retention} > "${persephone_test_logs}/persephone_tests.log" 2>&1
                      '''
                    } else {
                      if (env.JOB_NAME.contains(persephone_test_on_demand_job_name)){
                        // For the Persephone On Demand run, we built the agent locally.  So the agent is env.product_version, and
                        // the rest, which were pulled from Artifactory, are env.docker_tag.
                        tagAndPushDockerImage(env.internal_persephone_agent_repo, env.release_persephone_agent_repo, env.product_version)
                      }

                      // MR runs and Persephone on-demand runs use the local config service.
                      sh '''
                        echo "${PASSWORD}" | sudo -SE "${python}" main.py PersephoneTests --useLocalConfigService --dockerComposeFile ../docker/docker-compose-persephone.yml --resultsDir "${persephone_test_logs}" --keepBlockchains ${deployment_retention} > "${persephone_test_logs}/persephone_tests.log" 2>&1
                      '''
                    }
                  }
                }
              }
              saveTimeEvent("Persephone tests", "End")
            }catch(Exception ex){
              echo("A failure occurred while running the tests.")

              // See if this suite failed due to SR 19062354609.
              handleFailedTestSuite(env.persephone_test_logs)

              failRun()
              throw ex
            }
          }
        }
      }

      stage ("Post Memory Leak Testrun") {
        when {
          expression {
            env.JOB_NAME.contains(memory_leak_job_name) && params.run_tests
          }
        }
        stages {
          stage('Push memory leak summary into repo') {
            steps {
              script {
                try {
                  saveTimeEvent("Memory leak tasks", "Start")
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

                  saveTimeEvent("Memory leak tasks", "End")
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
          expression {
            env.JOB_NAME.contains(performance_test_job_name) && params.run_tests
          }
        }
        stages {
          stage('Collect Performance Transaction Rate') {
            steps {
              script {
                try {
                  saveTimeEvent("Performance tasks", "Start")
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

                  saveTimeEvent("Performance tasks", "End")
                } catch(Exception ex){
                    failRun()
                    throw ex
                }
              }
            }
          }
        }
      }

      stage("Finish tests"){
        steps {
          script {
            saveTimeEvent("Tests", "End")
          }
        }
      }

      stage("Save to artifactory"){
        when {
          expression {
            env.JOB_NAME.contains(env.tot_job_name) || params.deploy
          }
        }
        steps{
          script {
            try{
              saveTimeEvent("Save to artifactory", "Start")
              pushToArtifactory()
              saveTimeEvent("Save to artifactory", "End")
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
              saveTimeEvent("Push to DockerHub", "Start")
              dir('blockchain') {
                createAndPushGitTag(env.product_version)
              }

              tagImagesForRelease()
              pushToDockerHub()
              saveTimeEvent("Push to DockerHub", "End")

              dir('blockchain/vars') {
                script {
                  release_notification_address_file = "release_notification_recipients.txt"

                  if (fileExists(release_notification_address_file)) {
                    release_notification_recipients = readFile(release_notification_address_file).replaceAll("\n", " ")
                    emailext body: "Changes: \n" + getChangesSinceLastTag(),
                         to: release_notification_recipients,
                         subject: "[Build] Concord version " + env.product_version + " has been pushed to DockerHub."
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

      stage("Start an official Performance Run"){
        when {
          expression {
            env.JOB_NAME.contains(env.tot_job_name) &&
            currentBuild.currentResult == "SUCCESS"
          }
        }
        steps{
          script{
            startOfficialPerformanceRun()
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

          saveTimeEvent("Remove unnecessary docker artifacts", "Start")
          removeContainers()
          pruneImages()
          saveTimeEvent("Remove unnecessary docker artifacts", "End")

          if (!env.python) pythonlib.initializePython()

          racetrackSet("end")

          // Files created by the docker run belong to root because they were created by the docker process.
          // That will make the subsequent run unable to clean the workspace.  Just make the entire workspace dir
          // belong to builder to catch any future files.
          ownWorkspace()

          // Needs to trigger after owning workspace
          collectArtifacts()
          sendNotifications()
          cleanUpSDDCs()
        }
      }
    }
  }
}

// The user's parameter is top priority, and if it fails, let an exception be thrown.
// First, tries to fetch at branch_or_commit.
// Next, try to get the branch of the developer's MR which triggered this run.
// Failing the above, get ToT.
// Finally, if testing a commit, it merges the commit being tested into ToT of the
// current main branch (which is either master or a release branch).
// Returns the short form commit hash of the commit being tested.
String getRepoCode(repo_url, branch_or_commit, merge_branch_or_commit){
  refPrefix = "refs/heads/"
  gitlabRun = false

  if (branch_or_commit){
    branch_or_commit = branch_or_commit.trim()
  }

  echo("env.gitlabSourceBranch: " + env.gitlabSourceBranch)
  echo("branch_or_commit: " + branch_or_commit)

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
    mergeToTot(env.gitlabSourceBranch)
  }else if (merge_branch_or_commit && branch_or_commit){
    echo("Merging into ToT.")
    mergeToTot(branch_or_commit)
  }else{
    echo("Not merging into ToT.")
  }

  return commitBeingTested
}

// Returns whether this is a GitLab run.
Boolean isGitLabRun(){
  return env.gitlabSourceBranch && env.gitlabSourceBranch.trim()
}

// All that varies for each repo is the branch, so wrap this very large call.
// Note that this function is also in gitlablib.  We duplicate it here because
// the way we currently do master/tot/etc... runs, which I think we should change,
// those additional files have not been fetched yet.  Eventually, we should
// rework this.  See JenkinsfilePerformance for how.
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

// We are going to build/test against what things will look like after merge.
// These steps to merge were taken from GitLab, so we should be getting exactly
// what GitLab will get when it does a merge, unless someone else merges first.
void mergeToTot(branch_or_commit){
  if (branch_or_commit){
    sh(script: "git checkout ${branch_or_commit}")
    branchHash = getHead()

    sh(script: "git checkout ${tot_branch}")
    origTotHash = getHead()

    sh(script: "git merge --no-ff ${branch_or_commit} | tee '${env.WORKSPACE}/blockchain/vars/merge_output.log'")
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

// Given a repo and tag, pushes a docker image to whatever repo we
// are currently logged into (set up by the caller).  If tagAsLatest
// is set to true, that image will be re-tagged as latest and pushed
// again.
void pushDockerImage(repo, tag, tagAsLatest){
  // TESTING CODE ONLY
  // Push to a test repo in artifactory instead of the real one when testing push.
  // if (repo.contains("athena-docker-local")){
  //     old_repo = repo
  //     repo = repo.replace("athena-docker-local.artifactory.eng.vmware.com", "athena-docker-local.artifactory.eng.vmware.com/test")
  //     command = "docker tag ${old_repo}:${tag} ${repo}:${tag}"
  //     retryCommand(command, true)
  // }

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

// Tag orig_repo:docker_tag as new_repo:docker_tag and push new_repo:docker_tag to dockerhub.
void tagAndPushDockerImage(orig_repo, new_repo, docker_tag) {
  retryCommand("docker tag ${orig_repo}:${docker_tag} ${new_repo}:${docker_tag}", true)
  pushDockerImage(new_repo, docker_tag, false)
}

// Returns all changes since the last git tag.
String getChangesSinceLastTag(){
  return sh (
    script: 'git log `git tag -l --sort=-v:refname | head -n 1`..HEAD',
    returnStdout: true
  ).trim()
}

// Use groovy to create and return json for the version and commit
// for this run.  This gives the GUI something to display.
void createGUIVersionInfo(){
  versionObject = [:]
  versionObject.version = env.product_version
  versionObject.commit = env.commit
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
    script: "git pull --no-edit ; git push origin ${tot_branch}",
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
      echo 'ALERT: Performance Transaction Rate dipped in this run'

      performance_test_alert_notification_address_file = "performance_test_alert_recipients.txt"
      if (fileExists(performance_test_alert_notification_address_file)) {
        performance_test_alert_notification_recipients = readFile(performance_test_alert_notification_address_file).replaceAll("\n", " ")
        echo 'Sending ALERT email notification...'
        emailext body: "Performance Transaction Rate dipped in build: ${env.BUILD_NUMBER}\n\n More info at: ${env.BUILD_URL}\nPerformance Result Log file: ${env.BUILD_URL}artifact/testLogs/PerformanceTest/performance_result.log\n\nGraph: ${JOB_URL}plot",
        to: performance_test_alert_notification_recipients,
        subject: "ALERT: Performance Transaction Rate dipped in build ${env.BUILD_NUMBER}"
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

Boolean need_persephone_tests(always_exclude_jobs){
  paths_changed = have_any_paths_changed(['vars', 'buildall.sh', 'hermes', 'persephone', 'agent', 'concord'])
  needed_per_job_name = env.JOB_NAME.contains(env.tot_job_name) || env.JOB_NAME.contains("Blockchain Persephone Tests")

  // Never run for nightly runs which are focused on someting else.
  must_exclude = false
  for(always_exclude_job in always_exclude_jobs){
    if(env.JOB_NAME.contains(always_exclude_job)){
      must_exclude = true
    }
  }

  if(paths_changed){
    echo("One or more paths in the diff match the criteria for running Persephone tests.")
  }

  if(needed_per_job_name){
    echo("The job name matches the criteria for running Persephone tests.")
  }

  if(must_exclude){
    echo("Regardless of what other criteria say, job " + env.JOB_NAME + " will never run Persephone tests.")
    return false
  }else{
    return paths_changed || needed_per_job_name
  }
}

// Returns whether any of the passed in list of paths has changed.
// Paths should be relative to the root of the blockchain project.
Boolean have_any_paths_changed(paths){
  changed = false

  for(i = 0; i < paths.size() && !changed; ++i){
    dir(env.blockchain_root){
      absolute_path = new File(env.blockchain_root, paths[i]).toString()

      status = sh (
        script: "git diff origin/${tot_branch} --name-only --exit-code '${absolute_path}'",
        returnStatus: true
      )

      if(status != 0){
        // There was a change in git diff
        changed = true
      }
    }
  }

  return changed
}

// Remove all containers.
// Set returnStatus to true so that a build does not fail when there are no
// containers.  That should never happen anyway, but just in case.
void removeContainers(){
  echo "Removing docker containers"
  sh(script: '''docker rm -f $(docker ps -aq) > /dev/null''', returnStatus: true)
}

// Remove unused images.
void pruneImages(){
  echo "Pruning docker images"
  sh(script: "docker system prune --force > /dev/null", returnStatus: true)
}

// Report status about this system
void reportSystemStats(){
  echo "Jenkins node networking info:"
  sh(script:
  '''
  set +x
  echo
  ifconfig | grep -A 2 "ens"
  set -x
  ''')

  echo "Jenkins node disk stats:"
  sh(script: "df -h")

  echo "Jenkins node docker system stats:"
  sh(script: "docker system df")
}

// Returns whether we are pushing.  (If we are, the current run should run tests.)
Boolean runWillPush(){
  return env.JOB_NAME.contains(env.tot_job_name) || params.deploy
}

// If someone is trying to skip tests, and it is a run which pushes
// to a repo, make sure the user has entered a password.
void checkSkipTestsPassword(){
  withCredentials([string(credentialsId: 'BYPASS_JENKINS_TESTS', variable: 'PASSWORD')]) {
    if (runWillPush() && !params.run_tests) {
      msg = "The password to bypass tests is not correct.  It is required if bypassing tests "
      msg += "for a run which pushes to a repo such as Artifactory, Bintray, or Dockerhub"

      // Encrypt PASSWORD to compare it to the user's password.
      assert Secret.fromString(PASSWORD) == params.skip_tests_password : msg
    }
  }
}

void saveTimeEvent(stage, event){
  sh(script: "python3 \"${eventsRecorder}\" record_event '" + stage + "' '" + event + "' \"${eventsFullPath}\"")
}

void fetchSourceRepos() {
  echo "Fetch blockchain repo source"
  sh 'mkdir blockchain'
  dir('blockchain') {
    // After the checkout, the content of the repo is directly under 'blockchain'.
    // There is no extra 'vmwathena_blockchain' directory.
    env.commit = getRepoCode("git@gitlab.eng.vmware.com:blockchain/vmwathena_blockchain.git", params.blockchain_branch_or_commit, params.merge_branch_or_commit)
  }

  echo "Fetch VMware blockchain hermes-data source"
  sh 'mkdir hermes-data'
  dir('hermes-data') {
    env.actual_hermes_data_fetched = getRepoCode("git@gitlab.eng.vmware.com:blockchain/hermes-data", env.tot_branch, false)
    sh 'git checkout ${tot_branch}'
  }

  echo "Fetch samples from github"
  dir('blockchain') {
    sh '''
      git clone https://github.com/vmware-samples/vmware-blockchain-samples.git
      cd vmware-blockchain-samples
      git checkout 9711dda
      cd asset-transfer
      sed -i '27iRUN npm config set registry http://build-artifactory.eng.vmware.com:80/artifactory/api/npm/npm' Dockerfile
      cd ../supply-chain
      sed -i '12iRUN npm config set registry http://build-artifactory.eng.vmware.com:80/artifactory/api/npm/npm' Dockerfile
      cd ../..
    '''
  }
}

void pushToArtifactory(){
  pushList = [
    env.internal_asset_transfer_repo,
    env.internal_concord_repo,
    env.internal_ethrpc_repo,
    env.internal_fluentd_repo,
    env.internal_helen_repo,
    env.internal_memleak_concord_repo,
    env.internal_persephone_agent_repo,
    env.internal_persephone_configuration_repo,
    env.internal_persephone_ipam_repo,
    env.internal_persephone_provisioning_repo,
    env.internal_ui_repo,
    env.internal_contract_compiler_repo,
    env.internal_daml_ledger_api_repo,
    env.internal_daml_execution_engine_repo,
    env.internal_daml_index_db_repo,
    env.internal_hlf_orderer_base_repo,
    env.internal_hlf_peer_base_repo,
    env.internal_hlf_tools_base_repo,
    env.internal_hlf_orderer_repo,
    env.internal_hlf_peer_repo,
    env.internal_hlf_tools_repo,
    env.internal_trc_lib_repo,
    env.internal_trc_test_app_repo
  ]

  withCredentials([string(credentialsId: 'ARTIFACTORY_API_KEY', variable: 'ARTIFACTORY_API_KEY')]) {
    for (repo in pushList){
      // Short term, until we are sure the Jenkins job which sets up OneCloud
      // is working with version subdirs, we will push twice.
      // Always push the "old format" second, so it is newest, until all
      // tools are working with the new format.
      subdirRepo = repo + "/" + env.major_minor_patch
      command = "docker tag ${repo}:${env.docker_tag} ${subdirRepo}:${env.docker_tag}"
      retryCommand(command, true)

      pushDockerImage(subdirRepo, env.docker_tag, false)
      pushDockerImage(repo, env.docker_tag, false)
    }
  }
}


void pushToDockerHub(){
  pushList = [
    env.release_asset_transfer_repo,
    env.release_concord_repo,
    env.release_ethrpc_repo,
    env.release_fluentd_repo,
    env.release_helen_repo,
    env.release_persephone_agent_repo,
    // env.release_persephone_configuration_repo,
    // env.release_persephone_ipam_repo,
    // env.release_persephone_provisioning_repo,
    env.release_ui_repo,
    env.release_contract_compiler_repo,
    env.release_daml_ledger_api_repo,
    env.release_daml_execution_engine_repo,
    env.release_daml_index_db_repo,
    env.release_hlf_orderer_repo,
    env.release_hlf_peer_repo,
    env.release_hlf_tools_repo
  ]

  withCredentials([string(credentialsId: 'BLOCKCHAIN_REPOSITORY_WRITER_PWD', variable: 'DOCKERHUB_PASSWORD')]) {
    for (repo in pushList){
      pushDockerImage(repo, env.docker_tag, true)
    }
  }
}


void tagImagesForRelease(){
  sh(script:
  '''
    docker tag ${internal_asset_transfer_repo}:${docker_tag} ${release_asset_transfer_repo}:${docker_tag}
    docker tag ${internal_concord_repo}:${docker_tag} ${release_concord_repo}:${docker_tag}
    docker tag ${internal_ethrpc_repo}:${docker_tag} ${release_ethrpc_repo}:${docker_tag}
    docker tag ${internal_fluentd_repo}:${docker_tag} ${release_fluentd_repo}:${docker_tag}
    docker tag ${internal_helen_repo}:${docker_tag} ${release_helen_repo}:${docker_tag}
    docker tag ${internal_persephone_agent_repo}:${docker_tag} ${release_persephone_agent_repo}:${docker_tag}
    docker tag ${internal_persephone_configuration_repo}:${docker_tag} ${release_persephone_configuration_repo}:${docker_tag}
    docker tag ${internal_persephone_ipam_repo}:${docker_tag} ${release_persephone_ipam_repo}:${docker_tag}
    docker tag ${internal_persephone_provisioning_repo}:${docker_tag} ${release_persephone_provisioning_repo}:${docker_tag}
    docker tag ${internal_ui_repo}:${docker_tag} ${release_ui_repo}:${docker_tag}
    docker tag ${internal_contract_compiler_repo}:${docker_tag} ${release_contract_compiler_repo}:${docker_tag}
    docker tag ${internal_daml_ledger_api_repo}:${docker_tag} ${release_daml_ledger_api_repo}:${docker_tag}
    docker tag ${internal_daml_execution_engine_repo}:${docker_tag} ${release_daml_execution_engine_repo}:${docker_tag}
    docker tag ${internal_daml_index_db_repo}:${docker_tag} ${release_daml_index_db_repo}:${docker_tag}
    docker tag ${internal_hlf_orderer_repo}:${docker_tag} ${release_hlf_orderer_repo}:${docker_tag}
    docker tag ${internal_hlf_peer_repo}:${docker_tag} ${release_hlf_peer_repo}:${docker_tag}
    docker tag ${internal_hlf_tools_repo}:${docker_tag} ${release_hlf_tools_repo}:${docker_tag}
  ''')
}


// Will be turned on for good soon, after which we can get rid of this.
void enableTimeService(){
  sh(script: "sed -- \'s/\\(FEATURE_time_service: \\)false/\\1true/\' ../docker/config-public/dockerConfigurationInput.yaml > ../docker/config-public/dockerConfigurationInput-time_service.yaml")
}


// Called separately when running the UI tests, because UI automation cannot be launched with sudo.
void deleteDatabaseFiles(){
  echo("Entered deleteDatabaseFiles()")
  withCredentials([string(credentialsId: 'BUILDER_ACCOUNT_PASSWORD', variable: 'PASSWORD')]) {
    stdout = sh(script: '''
                 set -e
                 echo "${PASSWORD}" | sudo -S rm -rf ../docker/devdata/rocksdbdata*
                 echo "${PASSWORD}" | sudo -S rm -rf ../docker/devdata/postgresql
                ''',
                returnStdout: true)
  }
}


void runTests(){
  sh(script:
  '''
    # So test suites not using sudo can write to test_logs.
    rm -rf "${test_log_root}"
    mkdir "${test_log_root}"
  ''')

  echo("The following tests will be run in this invocation of runTests:")
  for (suite in testSuites.keySet()) {
    if (testSuites[suite].runSuite){
      echo("  " + suite)
    }
  }

  testGroups = createTestGroups()

  // Do everything via keySet().  Jenkins does not support iterating through a hashmap,
  // but we can iterate over a list of keys.
  for (group in testGroups.keySet()){
    echo("**** Starting group " + group + " ****")
    env.suiteNames = groups[group]["suites"].join(",")
    suiteParams = groups[group]["params"]

    env.suiteCmd = suiteParams.baseCommand ? suiteParams.baseCommand :
                 'echo ${PASSWORD} | sudo -SE ' + env.python + " main.py " + env.suiteNames
    env.suiteCmd += suiteParams.otherParameters ? " " + suiteParams.otherParameters : ""
    env.suiteRunConcordConfigGeneration = suiteParams.runConcordConfigurationGeneration == false ?
                                          "" : "--runConcordConfigurationGeneration"
    resultsDir = suiteParams.resultsDir ? suiteParams.resultsDir : group
    //GAAAH mixed case horror.
    env.suiteResultsDir = new File(env.test_log_root, resultsDir).toString()

    env.suitePerformanceVotes = suiteParams.performanceVotes ?
                              "--performanceVotes " + suiteParams.performanceVotes : ""
    env.suiteConcordConfigInput = suiteParams.concordConfigurationInput ?
                                "--concordConfigurationInput \"" + suiteParams.concordConfigurationInput + "\"" : ""
    env.suiteDockerComposeFiles = suiteParams.dockerComposeFiles ?
                                "--dockerComposeFile " + suiteParams.dockerComposeFiles : ""
    env.suiteDir = suiteParams.suiteDir ? suiteParams.suiteDir : "."
    env.groupLogFileName = group + ".log"

    if (suiteParams.setupFunction){
      setupFunction = suiteParams.setupFunction
      "$setupFunction"()
    }

    withCredentials([string(credentialsId: 'BUILDER_ACCOUNT_PASSWORD', variable: 'PASSWORD')]) {
      sh(script:
      '''
        EVENTS_FILE="${eventsFullPath}"

        # pushd is not available in the Jenkins shell.
        origDir=`pwd`
        cd "${suiteDir}"
        mkdir "${suiteResultsDir}"

        # Change to set -x for debugging the command.
        # Keep at +x for production.
        # set -x

        # Set +e so we can grep for the result summary instead of exiting immediately on failure.
        set +e

        eval "${suiteCmd} --resultsDir \\\"${suiteResultsDir}\\\" ${suiteRunConcordConfigGeneration} ${suitePerformanceVotes} ${suiteConcordConfigInput} ${suiteDockerComposeFiles} --logLevel debug" --eventsFile \\\"${EVENTS_FILE}\\\" > "${suiteResultsDir}/${groupLogFileName}" 2>&1
        suiteSuccess=$?

        # Print a line for jenkinslogs to search for.
        # Search for text output by hermes/main.py, processResults().
        set +x
        echo "Getting resultSummary"
        resultSummary=`grep -E "tests succeeded|tests failed|tests skipped" "${suiteResultsDir}/${groupLogFileName}"`
        echo Test result summary: "${resultSummary}"
        cd "${origDir}"

        # Now that we have finished our work, let the script fail.
        set -e
        if [ ${suiteSuccess} -ne 0 ]; then
           false
        fi
      '''
      )
    }
  }
}


Map createTestGroups(){
  groups = [:]

  for (suite in testSuites.keySet()) {
    if (testSuites[suite].runSuite){
      System.out.println("Finding or creating a group for suite: " + suite)

      inGroup = false

      if (!testSuites[suite].runAlone){
        deletedKey = null
        newMap = [:]

        if (!groups.isEmpty()){
          for (group in groups.keySet()){
            if (testSuites[suite] == groups[group]["params"]){
              inGroup = true
              groups[group]["suites"].add(suite)
              newMap[group + "_" + suite] = groups[group]
              deletedKey = group
              break
            }
          }

          if (inGroup) {
            groups.remove(deletedKey)
            groups = groups + newMap
          }
        }
      }

      if (!inGroup){
        groups["group_" + suite] = [
          "suites": [suite],
          "params": testSuites[suite]
        ]
      }

    }
  }

  return groups
}

// Use this function to generate a value to copy/paste into the Jenkins job.
// That job cannot read from here, so the next best thing is to just say
// NEVER MANUALLY CHANGE THOSE VALUES IN THE JENKINS JOB ITSELF BEYOND COPY/PASTE
// FROM THIS FUNCTION'S OUTPUT.
void printSelectableSuites(){
  echo("List of suites which can be selected: " + getSelectableSuites().toString())
}

String getSelectableSuites(){
  return testSuites.keySet()
}

void selectOnlySuites(desiredSuites){
  for (suite in testSuites.keySet()){
    found = suite in desiredSuites
    testSuites[suite].runSuite = found
  }
}

// Sets up a bunch of environment variables.
void setUpRepoVariables(){
  env.release_repo = "vmwblockchain"
  env.internal_repo_name = "athena-docker-local"
  env.internal_repo = env.internal_repo_name + ".artifactory.eng.vmware.com"

  // These are constants which mirror the DockerHub repos.  DockerHub is only used for publishing releases.
  env.release_asset_transfer_repo = env.release_repo + "/asset-transfer"
  env.release_concord_repo = env.release_repo + "/concord-core"
  env.release_ethrpc_repo = env.release_repo + "/ethrpc"
  env.release_fluentd_repo = env.release_repo + "/fluentd"
  env.release_helen_repo = env.release_repo + "/concord-ui"
  env.release_memleak_concord_repo = env.release_repo + "/memleak-concord-core"
  env.release_persephone_agent_repo = env.release_repo + "/agent"
  env.release_persephone_configuration_repo = env.release_repo + "/persephone-configuration"
  env.release_persephone_ipam_repo = env.release_repo + "/persephone-ipam"
  env.release_persephone_provisioning_repo = env.release_repo + "/persephone-provisioning"
  env.release_ui_repo = env.release_repo + "/ui"
  env.release_contract_compiler_repo = env.release_repo + "/contract-compiler"
  env.release_hlf_tools_repo = env.release_repo + "/hlf-tools"
  env.release_hlf_peer_repo = env.release_repo + "/hlf-peer"
  env.release_hlf_orderer_repo = env.release_repo + "/hlf-orderer"
  env.release_daml_ledger_api_repo = env.release_repo + "/daml-ledger-api"
  env.release_daml_execution_engine_repo = env.release_repo + "/daml-execution-engine"
  env.release_daml_index_db_repo = env.release_repo + "/daml-index-db"

  // These are constants which mirror the internal artifactory repos.  We put all merges
  // to master in the internal VMware artifactory.
  env.internal_asset_transfer_repo = env.release_asset_transfer_repo.replace(env.release_repo, env.internal_repo)
  env.internal_concord_repo = env.release_concord_repo.replace(env.release_repo, env.internal_repo)
  env.internal_ethrpc_repo = env.release_ethrpc_repo.replace(env.release_repo, env.internal_repo)
  env.internal_fluentd_repo = env.release_fluentd_repo.replace(env.release_repo, env.internal_repo)
  env.internal_helen_repo = env.internal_repo + "/helen"
  env.internal_memleak_concord_repo = env.release_memleak_concord_repo.replace(env.release_repo, env.internal_repo)
  env.internal_persephone_agent_repo = env.release_persephone_agent_repo.replace(env.release_repo, env.internal_repo)
  env.internal_persephone_configuration_repo = env.release_persephone_configuration_repo.replace(env.release_repo, env.internal_repo)
  env.internal_persephone_ipam_repo = env.release_persephone_ipam_repo.replace(env.release_repo, env.internal_repo)
  env.internal_persephone_provisioning_repo = env.release_persephone_provisioning_repo.replace(env.release_repo, env.internal_repo)
  env.internal_ui_repo = env.release_ui_repo.replace(env.release_repo, env.internal_repo)
  env.internal_contract_compiler_repo = env.release_contract_compiler_repo.replace(env.release_repo, env.internal_repo)
  env.internal_hlf_tools_repo = env.release_hlf_tools_repo.replace(env.release_repo, env.internal_repo)
  env.internal_hlf_peer_repo = env.release_hlf_peer_repo.replace(env.release_repo, env.internal_repo)
  env.internal_hlf_orderer_repo = env.release_hlf_orderer_repo.replace(env.release_repo, env.internal_repo)
  env.internal_hlf_tools_base_repo = env.internal_repo + "/fabric-tools"
  env.internal_hlf_peer_base_repo = env.internal_repo + "/fabric-peer"
  env.internal_hlf_orderer_base_repo = env.internal_repo + "/fabric-orderer"
  env.internal_daml_ledger_api_repo = env.release_daml_ledger_api_repo.replace(env.release_repo, env.internal_repo)
  env.internal_daml_execution_engine_repo = env.release_daml_execution_engine_repo.replace(env.release_repo, env.internal_repo)
  env.internal_daml_index_db_repo = env.release_daml_index_db_repo.replace(env.release_repo, env.internal_repo)

  // Note the Thin Replica Client Library (trc-lib) image is given only an
  // internal repo and not a release repo; this is because this image is not a
  // deliverable part of our product, but tagged builds of it may be used as
  // input to other product components.
  env.internal_trc_lib_repo = env.internal_repo + "/trc-lib"
  env.internal_trc_test_app_repo = env.internal_repo + "/trc-test-app"
}

// Carry out any special activities related to analysis or reporting
// when a suite fails.
// results_dir: Path to the test suite's results directory.
void handleFailedTestSuite(results_dir){
  lookForSR19062354609(results_dir)
}

void lookForSR19062354609(results_dir){
  found_match = false

  search_lines = [
    "Cannot find the storage backing of library S3-Subscriber",
    "due to the failure of importing file photon-ova.ovf",
    "Error exporting file photon-ova.ovf",
    "This error may occur due to restore of a deleted library"
  ]
  search_results_file = "SR19062354609.log"

  if (fileExists(results_dir)){
    echo("Searching directory '" + results_dir + "' for evidence of SR 19062354609.")

    for(int i = 0; i < search_lines.size(); ++i){
      search_line = search_lines[i]
      command = "grep -e '" + search_line + "' -r '" + results_dir + "' >> " + search_results_file
      status = sh(
        script: command,
        returnStatus: true
      )

      if(status == 0){
        echo("Found: '" + search_line + "'")
        found_match = true
        break
      }
    }
  }

  if(found_match){
    found_text = readFile(search_results_file)
    // Don't bother with the File object because readFile() dictates this format.
    recipients_file = "blockchain/vars/SR_19062354609_recipients.txt"
    recipients = readFile(recipients_file).replaceAll("\n", " ")
    msg = "Found an instance of SR 19062354609.\n" +
        "Build url: " + env.BUILD_URL + "\n" +
        "Text that led to this conclusion: " + found_text
    emailext body: msg,
             to: recipients,
             subject: "Instance of SR 19062354609 found!"
  }
}

// The version is: major.minor.patch.build
// The first three are read from build_info data file.
// The fourth digit is the Jenkins build number for
// the current run.
void setProductVersion(){
  raw_data = readFile("vars/build_info.json")
  version_object = new JsonSlurperClassic().parseText(raw_data)

  env.major_minor_patch = version_object["build_numbers"]["major"] + "." +
                          version_object["build_numbers"]["minor"] + "." +
                          version_object["build_numbers"]["patch"]
  env.product_version = env.major_minor_patch + "." + env.BUILD_NUMBER
}


String updateOneCloudProvisioningBintrayAndGetLatestTag(){
  saveTimeEvent("Build", "Fetch build number from Job Update-onecloud-provisioning-service")
  echo "For nightly runs (other than MR/ToT/Manual Run/ON DEMAND), fetching latest build number"
  def build = build job: 'Update-onecloud-provisioning-service', propagate: true, wait: true
  saveTimeEvent("Build", "Completed fetch build number from Job Update-onecloud-provisioning-service")
  return build.buildVariables.concord_tag
}

void setDockerTag(latest_docker_tag){
  env.recent_published_docker_tag = latest_docker_tag
  echo "Most recent build/tag: " + env.recent_published_docker_tag
  echo "Assigning '" + env.recent_published_docker_tag + "' to docker_tag"
  env.docker_tag = env.recent_published_docker_tag
}

// Takes care of sending all post-run notifications.
void sendNotifications(){
  sendGeneralEmail()

  if (currentBuild.currentResult == "FAILURE" &&
      env.JOB_NAME.contains(env.tot_job_name)){
    announceToTFailure()
  }
}


// Sends the general email for all run types, pass or fail.
void sendGeneralEmail(){
  echo 'Sending email notification...'

  body = "${currentBuild.currentResult}: Job ${env.JOB_NAME} build ${env.BUILD_NUMBER}\n " +
    "More info at: ${env.BUILD_URL}\n\nNOTE: Any failed persephone/helen deployment would be " +
    "retained for the next 1 hour, before cleanup."

  subject = "Jenkins Build ${currentBuild.currentResult}: Job ${env.JOB_NAME}"

  emailext body: body,
    recipientProviders: [[$class: 'DevelopersRecipientProvider'], [$class: 'RequesterRecipientProvider']],
    subject: subject
}


// When a ToT (e.g. master) run fails, notify users on Slack.
// Regarding email, I don't have a good solution for recipients yet.
// I am not sure if purnam-team is appropriate because of the non-engineering members.
// Getting users from GitLab would also not use the DA folks' "digitalassets" address,
// but maybe that's close enough.
// Getting all users who contributed to GitLab would include users who left the team.
void announceToTFailure(){
  // recipients = "purnam-team@vmware.com"
  // emailext to: recipients,
  //          subject: subject,
  //          body: env.run_fail_msg,
  //          presendScript: 'msg.addHeader("X-Priority", "1 (Highest)"); msg.addHeader("Importance", "High");'

  Random rnd = new Random()
  emoji_list = [":explode:", ":exploding_head:", ":fire:", ":bangbang:", ":alert:", ":boom:", ":boom1:", ":warning:",
                ":skull_and_crossbones:", ":jenkins-explode:"]
  emoji = emoji_list[rnd.nextInt(emoji_list.size)]
  subject = emoji + emoji + " <!here> Master run " + env.BUILD_NUMBER + " failed! " + emoji + emoji
  env.run_fail_msg = subject + "\nBuild url: " + env.BUILD_URL

  dir("blockchain/hermes"){
    sh '''
      . ${python_bin}/activate
      # For testing
      # python3 invoke.py slackDM   --param your_id@vmware.com "${run_fail_msg}"

      python3 invoke.py slackPost --param blockchain-build-fail "${run_fail_msg}"
      deactivate
    '''
  }
}

void announceSDDCCleanupFailures(failures){
  msg = "SDDC cleanup job(s) failed; it is possible resources are being leaked!"
  msg += "\nFailed jobs:"

  for(failure in failures){
    msg += "\n" + failure
  }

  env.cleanup_fail_msg = msg + "\nBuild url: " + env.BUILD_URL

  dir("blockchain/hermes"){
    sh '''
      . ${python_bin}/activate
      # For testing
      # python3 invoke.py slackDM --param your_id@vmware.com "${cleanup_fail_msg}"

      python3 invoke.py slackPost --param blockchain-build-fail "${cleanup_fail_msg}"
      deactivate
    '''
  }
}

void collectArtifacts(){
  saveTimeEvent("Gather artifacts", "Start")

  def logMap = [
    "tests": [
      "base": "**/testLogs/**/*.",
      "types": ["log", "csv", "txt", "json", "html", "png", "gz", "properties"]
    ],
    "builds": [
      "base": "**/blockchain/**/*.",
      "types": ["log", "json", "properties", "sql"]
    ]
  ]

  // Exclude 3rd party stuff; See BC-1858
  // node_modules; avoid bundling 3000+ package.json files
  def excludeMap = [
    "node_modules": [
      "pattern": "**/node_modules/**",
    ]
  ]

  excludedPaths = ""
  for (k in excludeMap.keySet()){
    if (excludedPaths != "") excludedPaths += ","
    excludedPaths += excludeMap[k]["pattern"]
  }

  // Iterate through the keys, *not* the map, because of Jenkins.
  for (k in logMap.keySet()){
    paths = ""
    for (logType in logMap[k]["types"]){
      if (paths != "") paths += ","
      paths += logMap[k]["base"] + logType
    }
    archiveArtifacts artifacts: paths, excludes: excludedPaths, allowEmptyArchive: true
  }

  saveTimeEvent("Gather artifacts", "End")

  // And grab the time file one more time so we can know how long gathering artifacts takes.
  archiveArtifacts artifacts: env.eventsFile, allowEmptyArchive: false
  archiveArtifacts artifacts: "**/failure_summary.log", allowEmptyArchive: true
  archiveArtifacts artifacts: "**/otherFailures/**", allowEmptyArchive: true
}

// Clean all SDDCs.
// Store all failures in an array and report them in one message.
void cleanUpSDDCs(){
  saveTimeEvent("Clean up SDDCs", "Start")
  sddcs = ['VMware-Blockchain-SDDC-1', 'VMware-Blockchain-SDDC-2', 'VMware-Blockchain-SDDC-3', 'VMware-Blockchain-SDDC-4']
  failures = []

  for(sddc in sddcs){
    failure = cleanSDDC(sddc, "HermesTesting", "1")
    if (failure){
      failures << failure
    }
  }

  // LongTests Clean up older than 168 hours (7 days)
  for(sddc in sddcs){
    failure = cleanSDDC(sddc, "HermesTesting-LongTests", "168")
    if (failure){
      failures << failure
    }
  }

  if (failures.size() > 0){
    announceSDDCCleanupFailures(failures)
  }

  saveTimeEvent("Clean up SDDCs", "End")
}

// Given an SDDC name, folder, and age, cleans it.
// Returns a string of information about the failure if it failed.
String cleanSDDC(sddc, folder, age){
  failure = null

  try{
    echo ("Calling Job Cleanup-SDDC-folder, SDDC: " + sddc + ", folder: " + folder + ", older than: " + age)
    build job: "Cleanup-SDDC-folder", parameters: [[$class: 'StringParameterValue', name: 'SDDC', value: sddc],
                                                   [$class: 'StringParameterValue', name: 'VMFolder', value: folder],
                                                   [$class: 'StringParameterValue', name: 'OLDERTHAN', value: age]]
  }catch(Exception ex){
    echo(ex.toString())
    failure = "SDDC: " + sddc + ", folder: " + folder
  }

  return failure
}

void racetrackSet(action){
  withCredentials([string(credentialsId: 'BUILDER_ACCOUNT_PASSWORD', variable: 'PASSWORD')]) {
    script {
      dir('blockchain/hermes') {
        if(action == "begin") {
          sh 'echo "${PASSWORD}" | sudo -SE "${python}" invoke.py racetrackSetBegin'
        } else if(action == "end"){
          env.run_result = currentBuild.currentResult
          sh 'echo "${PASSWORD}" | sudo -SE "${python}" invoke.py racetrackSetEnd --param "${run_result}"'
        }
      }
    }
  }
}

void ownWorkspace(){
  withCredentials([string(credentialsId: 'BUILDER_ACCOUNT_PASSWORD', variable: 'PASSWORD')]) {
    script {
      sh '''
        echo "${PASSWORD}" | sudo -S chown -R builder:builder .
      '''
    }
  }
}

void preprocessForMainMR(){
  // Figure out which components have changed based on git diff
  // From this information, you can:
  //    1) choose to pull pre-built images, instead of building everything
  //    2) skip component-internal unit tests of components that didn't change
  // Output the changed components map to `blockchain/vars/affected_components.json`
  // Also, extract effective commits and authors affecting this MR
  withCredentials([string(credentialsId: 'BUILDER_ACCOUNT_PASSWORD', variable: 'PASSWORD')]) {
    script {
      dir('blockchain/vars') {
        env.python = "/var/jenkins/workspace/venv_py37/bin/python"
        sh 'echo "${PASSWORD}" | sudo -SE "${python}" getChangedPaths.py'
        sh 'echo "${PASSWORD}" | sudo -SE "${python}" getCommitsBlame.py'
      }
    }
  }
}

void setEnvFileAndUserConfig(){
  withCredentials([
    string(credentialsId: 'BUILDER_ACCOUNT_PASSWORD', variable: 'PASSWORD'),
    string(credentialsId: 'JENKINS_JSON_API_KEY', variable: 'JENKINS_JSON_API_KEY'),
    string(credentialsId: 'LINT_API_KEY', variable: 'LINT_API_KEY'),
    string(credentialsId: 'FLUENTD_AUTHORIZATION_BEARER', variable: 'FLUENTD_AUTHORIZATION_BEARER'),
    string(credentialsId: 'VMC_API_TOKEN', variable: 'VMC_API_TOKEN'),
    string(credentialsId: 'WAVEFRONT_API_TOKEN', variable: 'WAVEFRONT_API_TOKEN'),
    string(credentialsId: 'DASHBOARD_WAVEFRONT_TOKEN', variable: 'DASHBOARD_WAVEFRONT_TOKEN'),
    string(credentialsId: 'SLACK_BOT_API_TOKEN', variable: 'SLACK_BOT_API_TOKEN'),
    string(credentialsId: 'VMW_DA_SLACK_BOT_API_TOKEN', variable: 'VMW_DA_SLACK_BOT_API_TOKEN'),
    string(credentialsId: 'LOG_INSIGHT_ON_ONECLOUD_PASSWORD', variable: 'LOG_INSIGHT_ON_ONECLOUD_PASSWORD'),
    usernamePassword(credentialsId: 'BINTRAY_CREDENTIALS', usernameVariable: 'BINTRAY_CONTAINER_REGISTRY_USERNAME', passwordVariable: 'BINTRAY_CONTAINER_REGISTRY_PASSWORD'),
    usernamePassword(credentialsId: 'DOCKERHUB_CREDENTIALS', usernameVariable: 'DOCKERHUB_REPO_READER_USERNAME', passwordVariable: 'DOCKERHUB_REPO_READER_PASSWORD'),
    usernamePassword(credentialsId: 'VMC_SDDC1_VC_CREDENTIALS', usernameVariable: 'VMC_SDDC1_VC_CREDENTIALS_USERNAME', passwordVariable: 'VMC_SDDC1_VC_CREDENTIALS_PASSWORD'),
    usernamePassword(credentialsId: 'VMC_SDDC2_VC_CREDENTIALS', usernameVariable: 'VMC_SDDC2_VC_CREDENTIALS_USERNAME', passwordVariable: 'VMC_SDDC2_VC_CREDENTIALS_PASSWORD'),
    usernamePassword(credentialsId: 'VMC_SDDC3_VC_CREDENTIALS', usernameVariable: 'VMC_SDDC3_VC_CREDENTIALS_USERNAME', passwordVariable: 'VMC_SDDC3_VC_CREDENTIALS_PASSWORD'),
    usernamePassword(credentialsId: 'VMC_SDDC4_VC_CREDENTIALS', usernameVariable: 'VMC_SDDC4_VC_CREDENTIALS_USERNAME', passwordVariable: 'VMC_SDDC4_VC_CREDENTIALS_PASSWORD'),
  ]) {
    sh '''
      echo "${PASSWORD}" | sudo -S ls
      sudo cat >blockchain/docker/.env <<EOF
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
memleak_concord_repo=${internal_memleak_concord_repo}
memleak_concord_tag=${docker_tag}
persephone_agent_repo=${internal_persephone_agent_repo}
persephone_agent_tag=${docker_tag}
persephone_configuration_repo=${internal_persephone_configuration_repo}
persephone_configuration_tag=${docker_tag}
persephone_ipam_repo=${internal_persephone_ipam_repo}
persephone_ipam_tag=${docker_tag}
persephone_provisioning_repo=${internal_persephone_provisioning_repo}
persephone_provisioning_tag=${docker_tag}
ui_repo=${internal_ui_repo}
ui_tag=${docker_tag}
contract_compiler_repo=${internal_contract_compiler_repo}
contract_compiler_tag=${docker_tag}
hlf_tools_repo=${internal_hlf_tools_repo}
hlf_tools_tag=${docker_tag}
hlf_peer_repo=${internal_hlf_peer_repo}
hlf_peer_tag=${docker_tag}
hlf_orderer_repo=${internal_hlf_orderer_repo}
hlf_orderer_tag=${docker_tag}
hlf_tools_base_repo=${internal_hlf_tools_base_repo}
hlf_tools_base_tag=${docker_tag}
hlf_peer_base_repo=${internal_hlf_peer_base_repo}
hlf_peer_base_tag=${docker_tag}
hlf_orderer_base_repo=${internal_hlf_orderer_base_repo}
hlf_orderer_base_tag=${docker_tag}
daml_ledger_api_repo=${internal_daml_ledger_api_repo}
daml_ledger_api_tag=${docker_tag}
daml_execution_engine_repo=${internal_daml_execution_engine_repo}
daml_execution_engine_tag=${docker_tag}
daml_index_db_repo=${internal_daml_index_db_repo}
daml_index_db_tag=${docker_tag}
trc_lib_repo=${internal_trc_lib_repo}
trc_lib_tag=${docker_tag}
trc_test_app_repo=${internal_trc_test_app_repo}
trc_test_app_tag=${docker_tag}
commit_hash=${commit}
LINT_API_KEY=${LINT_API_KEY}
LINT_AUTHORIZATION_BEARER=${FLUENTD_AUTHORIZATION_BEARER}
EOF
    '''

    updateEnvFileForConcordOnDemand()
    updateEnvFileForPersephoneOnDemand()

    sh '''
      cp blockchain/docker/.env blockchain/hermes/
      cp blockchain/docker/.env blockchain/vars/env.log
    '''

    env.JOB_NAME_ESCAPED = env.JOB_NAME.replaceAll('/', '___')
    env.WORKSPACE_ESCAPED = env.WORKSPACE.replaceAll('/', '___')
    sh '''
      # Update provisioning service application-test.properties
      sed -i -e 's/'"CHANGE_THIS_TO_HermesTesting"'/'"${PROVISIONING_SERVICE_NETWORK_NAME}"'/g' blockchain/hermes/resources/persephone/provisioning/app/profiles/application-test.properties
      sed -i -e 's/'"<JENKINS_JSON_API_KEY>"'/'"${JENKINS_JSON_API_KEY}"'/g' blockchain/hermes/resources/user_config.json
      sed -i -e 's/'"<VMC_API_TOKEN>"'/'"${VMC_API_TOKEN}"'/g' blockchain/hermes/resources/user_config.json
      sed -i -e 's/'"<WAVEFRONT_API_TOKEN>"'/'"${WAVEFRONT_API_TOKEN}"'/g' blockchain/hermes/resources/user_config.json
      sed -i -e 's/'"<WAVEFRONT_API_TOKEN>"'/'"${WAVEFRONT_API_TOKEN}"'/g' blockchain/docker/config-helen/app/db/migration/R__zone_entities.sql
      sed -i -e 's/'"<DASHBOARD_WAVEFRONT_TOKEN>"'/'"${DASHBOARD_WAVEFRONT_TOKEN}"'/g' blockchain/hermes/resources/user_config.json
      sed -i -e 's/'"<FLUENTD_AUTHORIZATION_BEARER>"'/'"${FLUENTD_AUTHORIZATION_BEARER}"'/g' blockchain/hermes/resources/user_config.json
      sed -i -e 's/'"<FLUENTD_AUTHORIZATION_BEARER>"'/'"${FLUENTD_AUTHORIZATION_BEARER}"'/g' blockchain/docker/config-helen/app/db/migration/R__zone_entities.sql
      sed -i -e 's/'"<VMC_SDDC1_VC_CREDENTIALS_USERNAME>"'/'"${VMC_SDDC1_VC_CREDENTIALS_USERNAME}"'/g' blockchain/hermes/resources/user_config.json
      sed -i -e 's/'"<VMC_SDDC1_VC_CREDENTIALS_PASSWORD>"'/'"${VMC_SDDC1_VC_CREDENTIALS_PASSWORD}"'/g' blockchain/hermes/resources/user_config.json
      sed -i -e 's/'"<VMC_SDDC2_VC_CREDENTIALS_USERNAME>"'/'"${VMC_SDDC2_VC_CREDENTIALS_USERNAME}"'/g' blockchain/hermes/resources/user_config.json
      sed -i -e 's/'"<VMC_SDDC2_VC_CREDENTIALS_PASSWORD>"'/'"${VMC_SDDC2_VC_CREDENTIALS_PASSWORD}"'/g' blockchain/hermes/resources/user_config.json
      sed -i -e 's/'"<VMC_SDDC3_VC_CREDENTIALS_USERNAME>"'/'"${VMC_SDDC3_VC_CREDENTIALS_USERNAME}"'/g' blockchain/hermes/resources/user_config.json
      sed -i -e 's/'"<VMC_SDDC3_VC_CREDENTIALS_PASSWORD>"'/'"${VMC_SDDC3_VC_CREDENTIALS_PASSWORD}"'/g' blockchain/hermes/resources/user_config.json
      sed -i -e 's/'"<VMC_SDDC4_VC_CREDENTIALS_USERNAME>"'/'"${VMC_SDDC4_VC_CREDENTIALS_USERNAME}"'/g' blockchain/hermes/resources/user_config.json
      sed -i -e 's/'"<VMC_SDDC4_VC_CREDENTIALS_PASSWORD>"'/'"${VMC_SDDC4_VC_CREDENTIALS_PASSWORD}"'/g' blockchain/hermes/resources/user_config.json
      sed -i -e 's/'"<METAINF_ENV_JOB_NAME>"'/'"${JOB_NAME_ESCAPED}"'/g' blockchain/hermes/resources/user_config.json
      sed -i -e 's/'"<METAINF_ENV_BUILD_NUMBER>"'/'"${BUILD_NUMBER}"'/g' blockchain/hermes/resources/user_config.json
      sed -i -e 's/'"<METAINF_ENV_DOCKER_TAG>"'/'"${docker_tag}"'/g' blockchain/hermes/resources/user_config.json
      sed -i -e 's/'"<METAINF_ENV_WORKSPACE>"'/'"${WORKSPACE_ESCAPED}"'/g' blockchain/hermes/resources/user_config.json
      sed -i -e 's/'"<SLACK_BOT_API_TOKEN>"'/'"${SLACK_BOT_API_TOKEN}"'/g' blockchain/hermes/resources/user_config.json
      sed -i -e 's/'"<VMW_DA_SLACK_BOT_API_TOKEN>"'/'"${VMW_DA_SLACK_BOT_API_TOKEN}"'/g' blockchain/hermes/resources/user_config.json
      sed -i -e 's/'"<LOG_INSIGHT_ON_ONECLOUD_PASSWORD>"'/'"${LOG_INSIGHT_ON_ONECLOUD_PASSWORD}"'/g' blockchain/hermes/resources/user_config.json
    '''

    if (env.JOB_NAME.contains(long_tests_job_name)) {
      sh '''
        sed -i -e 's/'"<DEPLOYMENT_FOLDER>"'/'"HermesTesting-LongTests"'/g' blockchain/hermes/resources/user_config.json
        sed -i -e 's/'"<MONITORING_NOTIFICATION_RUN_NAME>"'/'"Long-running test"'/g' blockchain/hermes/resources/user_config.json
        sed -i -e 's/'"<MONITORING_NOTIFICATION_TARGET>"'/'"blockchain-long-tests-status"'/g' blockchain/hermes/resources/user_config.json
      '''
    } else {
      sh '''
        sed -i -e 's/'"<DEPLOYMENT_FOLDER>"'/'"HermesTesting"'/g' blockchain/hermes/resources/user_config.json
      '''
    }

    if (env.JOB_NAME.contains(persephone_test_job_name)) {
      sh '''
        # Update provisioning service application-test.properties with bintray registry for nightly runs
        sed -i -e 's!'"<CONTAINER_REGISTRY_ADDRESS>"'!'"${BINTRAY_CONTAINER_REGISTRY_ADDRESS}"'!g' blockchain/hermes/resources/persephone/provisioning/app/profiles/application-test.properties
        sed -i -e 's/'"<CONTAINER_REGISTRY_USERNAME>"'/'"${BINTRAY_CONTAINER_REGISTRY_USERNAME}"'/g' blockchain/hermes/resources/persephone/provisioning/app/profiles/application-test.properties
        sed -i -e 's/'"<CONTAINER_REGISTRY_PASSWORD>"'/'"${BINTRAY_CONTAINER_REGISTRY_PASSWORD}"'/g' blockchain/hermes/resources/persephone/provisioning/app/profiles/application-test.properties
      '''
    } else {
      sh '''
        # Update provisioning service application-test.properties with dockerhub registry for non-nightly runs
        sed -i -e 's!'"<CONTAINER_REGISTRY_ADDRESS>"'!'"${DOCKERHUB_CONTAINER_REGISTRY_ADDRESS}"'!g' blockchain/hermes/resources/persephone/provisioning/app/profiles/application-test.properties
        sed -i -e 's/'"<CONTAINER_REGISTRY_USERNAME>"'/'"${DOCKERHUB_REPO_READER_USERNAME}"'/g' blockchain/hermes/resources/persephone/provisioning/app/profiles/application-test.properties
        sed -i -e 's/'"<CONTAINER_REGISTRY_PASSWORD>"'/'"${DOCKERHUB_REPO_READER_PASSWORD}"'/g' blockchain/hermes/resources/persephone/provisioning/app/profiles/application-test.properties
      '''
    }
  }
}

void updateEnvFileForConcordOnDemand(){
  script {
    if (env.JOB_NAME.contains(deploy_concord_job_name)) {
      if (env.concord_deployment_tag.toLowerCase() == "lastsuccessfultot") {
        env.dep_comp_concord_tag = env.docker_tag
        sh '''
          echo "Using concord tag (last successful ToT): " + ${dep_comp_concord_tag}
        '''
      }
      else if (env.concord_deployment_tag.toLowerCase() == "thisbranch") {
        env.dep_comp_concord_tag = env.product_version
        sh '''
          echo "Using concord tag (from this branch): " + ${dep_comp_concord_tag}
          sed -i -e 's/'"concord_tag=${docker_tag}"'/'"concord_tag=${dep_comp_concord_tag}"'/g' blockchain/docker/.env
        '''
      } else {
        env.dep_comp_concord_tag = env.concord_deployment_tag
        sh '''
          echo "Using concord tag (custom tag):" + ${dep_comp_concord_tag}
          sed -i -e 's/'"concord_tag=${docker_tag}"'/'"concord_tag=${dep_comp_concord_tag}"'/g' blockchain/docker/.env
        '''
      }
    }
  }
}

// The Performance team has requested we kick off a job on their server for every successful
// master run.  We will not wait for it or allow it to fail a run.
void startOfficialPerformanceRun(){
  try{
    dir('blockchain'){
      performance.startPerformanceRun()
    }
  }catch(Exception ex){
    echo("Exception while starting a performance run:")
    echo(ex.toString())
    echo("A failure to launch the performance run will not fail a build.")
  }
}

// Re-tag athena-docker-local.artifactory.eng.vmware.com:foo to vmwblockchain:foo.
// "vmwblockchain" is what we call it on DockerHub.
// This is needed for deployment testing, as SDDCs are located outside the firewall.
void pushConcordComponentsToDockerHub(){
  tagAndPushDockerImage(env.internal_persephone_agent_repo, env.release_persephone_agent_repo, env.docker_tag)
  tagAndPushDockerImage(env.internal_persephone_agent_repo, env.release_persephone_agent_repo, env.docker_tag)
  tagAndPushDockerImage(env.internal_concord_repo, env.release_concord_repo, env.docker_tag)
  tagAndPushDockerImage(env.internal_ethrpc_repo, env.release_ethrpc_repo, env.docker_tag)
  tagAndPushDockerImage(env.internal_daml_ledger_api_repo, env.release_daml_ledger_api_repo, env.docker_tag)
  tagAndPushDockerImage(env.internal_daml_execution_engine_repo, env.release_daml_execution_engine_repo, env.docker_tag)
  tagAndPushDockerImage(env.internal_daml_index_db_repo, env.release_daml_index_db_repo, env.docker_tag)
  tagAndPushDockerImage(env.internal_fluentd_repo, env.release_fluentd_repo, env.docker_tag)
}

// For the Persephone On Demand, we only build and want to use
// Persephone components.
void updateEnvFileForPersephoneOnDemand(){
  if (env.JOB_NAME.contains(persephone_test_on_demand_job_name)){
    setPropVals([
      "persephone_agent_tag",
      "persephone_configuration_tag",
      "persephone_ipam_tag",
      "persephone_provisioning_tag"
    ], env.product_version, "blockchain/docker/.env")
  }
}

// Sets a list of keys equal to val in file.
void setPropVals(keys, val, file){
  for (key in keys){
    echo "Setting '" + key + "' to '" + val + "' in " + file
    env.setPropValsKey = key
    env.setPropValsVal = val
    env.setPropValsFile = file
    sh '''
      sed -i -e 's/'"${setPropValsKey}=.*"'/'"${setPropValsKey}=${setPropValsVal}"'/g' "${setPropValsFile}"
    '''
    contents = readFile(file)
    echo("File now contains: " + contents)
  }
}
