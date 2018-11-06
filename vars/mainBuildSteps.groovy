def call(){
  pipeline {
    agent any
    tools {
        nodejs 'Node 8.9.1'
    }
    parameters {
      booleanParam defaultValue: false, description: 'If tests pass, deploy the docker images for production', name: 'deploy'
      string defaultValue: '',
             description: 'The version number for releases. Used as a tag in DockerHub and GitHub.',
             name: 'version_param'

      string defaultValue: '',
             description: 'Blockchain commit or branch to use.  Providing a branch name will pull the branch\'s latest commit.',
             name: 'blockchain_branch_or_commit'
      string defaultValue: '',
             description: 'Shared Jenkins lib branch to use.',
             name: 'shared_lib_branch'
    }
    stages {
      stage('Clean') {
        steps {
          cleanWs()
        }
      }
      stage('Fetch source code') {
        parallel {
          stage('Fetch blockchain repo source') {
            steps {
              sh 'mkdir blockchain'
              dir('blockchain') {
                script {
                  env.actual_blockchain_fetched = getRepoCode("git@github.com:vmwathena/blockchain.git", params.blockchain_branch_or_commit)
                }
              }
            }
          }
        }
      }

      stage('Copy dependencies') {
        parallel {
          stage('Copy googletest') {
            steps() {
              sh 'mkdir googletest'
              dir('googletest') {
                sh 'cp -ar /var/jenkins/workspace/googletest/* .'
              }
            }
          }
          stage('Copy evmjit') {
            steps() {
              sh 'mkdir evmjit'
              dir('evmjit') {
                sh 'cp -ar /var/jenkins/workspace/evmjit/* .'
              }
            }
          }
          stage('Copy etherium tests') {
            steps() {
              sh 'mkdir ethereum_tests'
              dir('ethereum_tests') {
                sh 'cp -ar /var/jenkins/workspace/ethereum_tests/* .'
              }
            }
          }
        }
      }

      stage('Build products') {
        parallel {
          stage('Build Athena') {
            steps {
              dir('blockchain/athena') {
                sh '''currentDir=`pwd`
                sed -i\'\' "s?/tmp/genesis.json?${currentDir}/test/resources/genesis.json?g" resources/athena1.config
                sed -i\'\' "s?/tmp/genesis.json?${currentDir}/test/resources/genesis.json?g" resources/athena2.config
                sed -i\'\' "s?/tmp/genesis.json?${currentDir}/test/resources/genesis.json?g" resources/athena3.config
                sed -i\'\' "s?/tmp/genesis.json?${currentDir}/test/resources/genesis.json?g" resources/athena4.config

                git submodule init
                git submodule update --recursive
                mkdir -p build
                cd build
                cmake ..
                make'''
              }
            }
          }
          stage('Build Helen') {
            steps {
              dir('blockchain/helen') {
                sh 'mvn clean install package'
              }
            }
          }
        }
      }
      stage('Run tests') {
        steps {
          dir('blockchain/hermes') {
            sh './main.py CoreVMTests'
            sh './main.py HelenAPITests'
            sh './main.py ExtendedRPCTests'
            sh './main.py RegressionTests'
          }
        }
      }

      stage('Configure docker and git') {
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

          // Log into docker.  Does it expire?  Try doing it once, here, and see what happens.
          withCredentials([string(credentialsId: 'BLOCKCHAIN_REPOSITORY_WRITER_PWD', variable: 'DOCKERHUB_PASSWORD')]) {

            sh '''
              echo "${DOCKERHUB_PASSWORD}" | docker login -u blockchainrepositorywriter --password-stdin
            '''
          }

          // To invoke "git tag" and commit that change, git wants to know who we are.
          // This will be set up in template VM version 5, at which point these commands can
          // be removed.
          sh '''
            git config --global user.email "vmwathenabot@vmware.com"
            git config --global user.name "build system"
          '''

          // These are constants which mirror the DockerHub repos.
          script {
            env.athena_repo = 'vmwblockchain/concord-core'
            env.helen_repo = 'vmwblockchain/concord-ui'
          }
        }
      }

      stage('Build docker images') {
        parallel {
          stage('Build helen docker image') {
            steps {
              script {
                dir('blockchain/helen') {

                 script {
                    env.helen_docker_tag = env.version_param ? env.version_param : env.actual_blockchain_fetched
                  }

                  withCredentials([string(credentialsId: 'BUILDER_ACCOUNT_PASSWORD', variable: 'PASSWORD')]) {
                    sh '''
                      docker build . -t "${helen_repo}:${helen_docker_tag}"
                    '''
                  }
                }
              }
            }
          }


          stage('Build athena docker image') {
            steps {
              script {
                dir('blockchain/athena') {

                  script {
                    env.athena_docker_tag = env.version_param ? env.version_param : env.actual_blockchain_fetched
                  }
                  withCredentials([string(credentialsId: 'BLOCKCHAIN_REPOSITORY_WRITER_PWD', variable: 'DOCKERHUB_PASSWORD')]) {
                    sh '''
                      ./docker-build.sh "${athena_repo}" "${athena_docker_tag}"
                    '''
                  }
                }
              }
            }
          }
        }
      }

      stage('Run tests in containers') {
        steps {
          dir('blockchain/hermes'){
            withCredentials([string(credentialsId: 'BUILDER_ACCOUNT_PASSWORD', variable: 'PASSWORD')]) {
              sh '''
                echo "${PASSWORD}" | sudo -S ls
                sudo cat >.env <<EOL
athena_repo=${athena_repo}
athena_tag=${athena_docker_tag}
athena_repo=${helen_repo}
athena_repo=${helen_docker_tag}
                EOL
              '''

              sh 'sudo -S ./main.py ExtendedRPCTests --dockerComposeFile ../athena/docker/docker-compose.yml'
            }
          }
        }
      }

      // stage('Clean containers') {
      //   steps {
      //     dir('docker') {
      //       sh "docker-compose down"
      //     }
      //   }
      // }

      stage('Push to docker repository') {
        when {
          environment name: 'deploy', value: 'true'
        }
        steps {
          dir('blockchain') {
            createAndPushTag(env.version_param)
          }
          withCredentials([string(credentialsId: 'BLOCKCHAIN_REPOSITORY_WRITER_PWD', variable: 'DOCKERHUB_PASSWORD')]) {
            sh '''
              docker push ${athena_repo}:${version_param}
              docker tag ${athena_repo}:${version_param} ${athena_repo}:latest
              docker push ${athena_repo}:latest

              docker push ${helen_repo}:${version_param}
              docker tag ${helen_repo}:${version_param} ${helen_repo}:latest
              docker push ${helen_repo}:latest
            '''
          }
        }
      }
    }// End stages

    post {
      always {
        archiveArtifacts artifacts: '**/*.log', allowEmptyArchive: true
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

// Creates a git tag and commits it. Must be called when the pwd is the
// source git directory.
void createAndPushTag(tag){
  sh (
    script: "git tag -a ${tag} -m 'Version tag created by the build system'",
    returnStdout: false
  )

  sh (
    script: "git push origin ${tag}",
    returnStdout: false
  )
}
