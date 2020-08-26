import groovy.json.JsonOutput
import groovy.json.JsonSlurperClassic
import groovy.transform.Field

// Pipeline env

@Field String agentLabel = "kashTestNode"

def call() {
  env.tot_job_name = "Master Branch"
  env.tot_branch = "master"

  pipeline {
    agent { label agentLabel }
    tools {
      nodejs 'Node 11.15.0'
    }
    options {
      gitLabConnection('TheGitlabConnection')
    }
    parameters {
      string defaultValue: "",
             description: "Blockchain commit or branch to use. Providing a branch name will pull the branch's latest commit.",
             name: "blockchain_branch_or_commit"

      string defaultValue: "",
             description: "Shared Jenkins lib branch to use.",
             name: "shared_lib_branch"
    }
    stages {

      stage("Infra Setup") {
        steps {
          script {
            try {
              gitlablib = load "vars/util/gitlablib.groovy"
              pythonlib = load "vars/util/pythonlib.groovy"
              jenkinsbuilderlib = load "vars/util/jenkinsbuilderlib.groovy"
              artifactorylib = load "vars/util/artifactorylib.groovy"
              dockerutillib = load "vars/util/dockerutillib.groovy"
              customathenautil = load "vars/athenaspecific/customathenautil.groovy"

              dockerutillib.removeContainers()

              jenkinsbuilderlib.reportSystemStats()

              // chown files in case something was left behind owned by root, so we can cleanWs().
              jenkinsbuilderlib.ownWorkspace()

              cleanWs()

              // Add the VMware GitLab ssh key to known_hosts.
              customathenautil.handleKnownHosts("gitlab.eng.vmware.com")

              // Try dealing with https://issues.jenkins-ci.org/browse/JENKINS-48300. Run failed with this error:
              // "JENKINS-48300: if on an extremely laggy filesystem, consider -Dorg.jenkinsci.plugins.durabletask.BourneShellScript.HEARTBEAT_CHECK_INTERVAL=86400"
              System.setProperty("org.jenkinsci.plugins.durabletask.BourneShellScript.HEARTBEAT_CHECK_INTERVAL",
                                 "86400")

            } catch (Exception ex) {
              failRun(ex)
              throw ex
            }
          }
        }
      }

      stage("Configure docker, git, and python") {
        steps {
          script {
            try {
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

              withCredentials([string(credentialsId: 'ATHENA_DEPLOYER_ARTIFACTORY_PASSWORD',
                                      variable: 'ARTIFACTORY_PASSWORD')]) {
                script {
                  command = "docker login -u athena-deployer -p \"" + env.ARTIFACTORY_PASSWORD +
                            "\" athena-docker-local.artifactory.eng.vmware.com"
                  jenkinsbuilderlib.retryCommand(command, true)
                }
              }

              withCredentials(
                      [string(credentialsId: 'BLOCKCHAIN_REPOSITORY_WRITER_PWD', variable: 'DOCKERHUB_PASSWORD')]) {
                script {
                  command = "docker login -u blockchainrepositorywriter -p " + env.DOCKERHUB_PASSWORD
                  jenkinsbuilderlib.retryCommand(command, true)
                }
              }

              // To invoke "git tag" and commit that change, git wants to know who we are.
              // This will be set up in template VM version 5, at which point these commands can
              // be removed.
              sh '''
                git config --global user.email "vmwathenabot@vmware.com"
                git config --global user.name "build system"
              '''

              echo "This run requires building components"
              env.docker_tag = "kash-" + env.product_version

              setUpRepoVariables()

            } catch (Exception ex) {
              println("Unable to set up environment: ${ex}")
              failRun(ex)
              throw ex
            }
          }
        }
      }

      // Custom code for the job
      stage("Checkout Source Code") {
        steps {
          script {
            try {
              script {
                fetchSourceRepos()
                env.blockchain_root = new File(env.WORKSPACE, "blockchain").toString()
              }
            } catch (Exception ex) {
              failRun(ex)
              throw ex
            }
          }
        }
      }

      stage("Build") {
        steps {
          script {
            try {
              racetrackUpdate(repo: "bcs", action: "setBegin", type: "building")
              dir('blockchain') {
                sh '''
                    echo "Building All components..."
                    ./buildbcs.sh
                  '''
              }
              racetrackUpdate(repo: "bcs", action: "setEnd", type: "building", result: "SUCCESS")
            } catch (Exception ex) {
              failRun(ex)
              racetrackUpdate(repo: "bcs", action: "setEnd", type: "building", result: "FAILURE")
              throw ex
            }
          }
          archiveArtifacts artifacts: env.eventsFile, allowEmptyArchive: false
        }
      }

      // TODO should be part of build itself.
      stage("Save to artifactory") {
        steps {
          script {
            try {
              pushToArtifactory()
            } catch (Exception ex) {
              failRun(ex)
              throw ex
            }
          }
        }
      }

    }

    post {
        always {
          script {

            jenkinsbuilderlib.retryCommand("docker logout", false)
            jenkinsbuilderlib.retryCommand("docker logout athena-docker-local.artifactory.eng.vmware.com", false)

            dir('blockchain') {
              dockerutillib.removeContainers()
              dockerutillib.removeImages()
            }

            jenkinsbuilderlib.ownWorkspace()
            collectArtifacts()
          }
        }
      }
    }
}

void fetchSourceRepos() {
  echo "Fetch blockchain repo source"
  sh 'mkdir blockchain'
  dir('blockchain') {
    // After the checkout, the content of the repo is directly under 'blockchain'.
    // There is no extra 'vmwathena_blockchain' directory.
    env.commit = gitlablib.getRepoCode("git@gitlab.eng.vmware.com:blockchain/vmwathena_blockchain.git", "khank/ci.saas.2", params.merge_branch_or_commit,
                                       '${env.WORKSPACE}/blockchain/vars/merge_output.log')
  }
}

void pushToArtifactory(){
  pushList = [
    env.internal_fluentd_repo,
    env.internal_helen_repo,
    env.internal_persephone_agent_repo,
    env.internal_persephone_configuration_repo,
    env.internal_persephone_ipam_repo,
    env.internal_persephone_provisioning_repo,
    env.internal_ui_repo,
  ]

  withCredentials([string(credentialsId: 'ARTIFACTORY_API_KEY', variable: 'ARTIFACTORY_API_KEY')]) {
    for (repo in pushList){
      // Short term, until we are sure the Jenkins job which sets up OneCloud
      // is working with version subdirs, we will push twice.
      // Always push the "old format" second, so it is newest, until all
      // tools are working with the new format.
      subdirRepo = repo + "/" + env.major_minor_patch
      command = "docker tag ${repo}:${env.BUILD_NUMBER} ${subdirRepo}:${env.docker_tag}"
      jenkinsbuilderlib.retryCommand(command, true)

      pushDockerImage('Master-Artifact-Subdir', subdirRepo, env.docker_tag, false)
      pushDockerImage('Master-Artifact', repo, env.docker_tag, false)
    }
  }
}

//TODO Change them to a standard pattern.
// Sets up a bunch of environment variables.
void setUpRepoVariables(){
  env.release_repo = "vmwblockchain"
  env.internal_repo_name = "athena-docker-local"
  env.internal_repo = env.internal_repo_name + ".artifactory.eng.vmware.com"

  // These are constants which mirror the DockerHub repos.  DockerHub is only used for publishing releases.
  env.release_fluentd_repo = env.release_repo + "/fluentd"
  env.release_persephone_agent_repo = env.release_repo + "/agent"
  env.release_persephone_configuration_repo = env.release_repo + "/persephone-configuration"
  env.release_persephone_ipam_repo = env.release_repo + "/persephone-ipam"
  env.release_persephone_provisioning_repo = env.release_repo + "/persephone-provisioning"
  env.release_ui_repo = env.release_repo + "/ui"

  // These are constants which mirror the internal artifactory repos.  We put all merges in internal VMware artifactory.
  env.internal_fluentd_repo = env.release_fluentd_repo.replace(env.release_repo, env.internal_repo)
  env.internal_helen_repo = env.internal_repo + "/helen"
  env.internal_persephone_agent_repo = env.release_persephone_agent_repo.replace(env.release_repo, env.internal_repo)
  env.internal_persephone_configuration_repo = env.release_persephone_configuration_repo.replace(env.release_repo, env.internal_repo)
  env.internal_persephone_ipam_repo = env.release_persephone_ipam_repo.replace(env.release_repo, env.internal_repo)
  env.internal_persephone_provisioning_repo = env.release_persephone_provisioning_repo.replace(env.release_repo, env.internal_repo)
  env.internal_ui_repo = env.release_ui_repo.replace(env.release_repo, env.internal_repo)

}

void racetrackUpdate(Map params){
  withCredentials([string(credentialsId: 'BUILDER_ACCOUNT_PASSWORD', variable: 'PASSWORD')]) {
    def action = params.action
    env.set_type = params.type ? params.type : "testing" // "building" | "testing"
    // future-proofing for repo split
    env.repo = params.repo ? params.repo : "athena" // "athena" | "concord" | ...
    script {
      dir('blockchain/hermes') {
        try {
          if (action == "setBegin") {
            sh 'echo "${PASSWORD}" | sudo -SE "${python}" invoke.py racetrackSetBegin --param "${repo}" "${set_type}"'  
          } else if (action == "setEnd") {
            env.run_result = params.result ? params.result : "FAILURE"
            sh 'echo "${PASSWORD}" | sudo -SE "${python}" invoke.py racetrackSetEnd --param "${repo}" "${run_result}" "${set_type}"'
          }
        } catch (Exception ex) {
          echo("Racetrack update operation has failed: " + ex.toString())
        }
      }
    }
  }
}
