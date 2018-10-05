def call(){
  pipeline {
    agent any
    tools {
        nodejs 'Node 8.9.1'
    }
    parameters {
      booleanParam defaultValue: false, description: 'If tests pass, deploy the docker images for production', name: 'Deploy'
      string defaultValue: '',
             description: 'Athena commit or branch to use.  Providing a branch name will pull the branch\'s latest commit.',
             name: 'athena_branch_or_commit'
      string defaultValue: '',
             description: 'Helen commit or branch to use.  Providing a branch name will pull the branch\'s latest commit.',
             name: 'helen_branch_or_commit'
      string defaultValue: '',
             description: 'Hermes commit or branch to use.  Providing a branch name will pull the branch\'s latest commit.',
             name: 'hermes_branch_or_commit'
    }
    stages {
      stage('Clean') {
        steps {
          cleanWs()
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
      // stage('test email') {
      //   steps() {
      //     script {
      //       emailext (
      //         subject: "STARTED: Job '${env.JOB_NAME} [${env.BUILD_NUMBER}]'",
      //         body: """<p>STARTED: Job '${env.JOB_NAME} [${env.BUILD_NUMBER}]':</p>
      //           <p>Check console output at "<a href="${env.BUILD_URL}">${env.JOB_NAME} [${env.BUILD_NUMBER}]</a>"</p>""",
      //         recipientProviders: [[$class: 'DevelopersRecipientProvider']],
      //         to: "rvollmar@vmware.com"
      //       )
      //     }
      //   }
      // }
      stage('Build products') {
        parallel {
          stage('Build Athena') {
            steps {
              echo "Athena branch/commit: ${params.athena_branch_or_commit}"
              sh 'mkdir athena'
              dir('athena') {
                getRepoCode("https://github.com/vmwathena/athena", params.athena_branch_or_commit)
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
              echo "Helen branch/commit: ${params.helen_branch_or_commit}"
              sh 'mkdir helen'
              dir('helen') {
                getRepoCode("https://github.com/vmwathena/helen", params.helen_branch_or_commit)
                sh 'mvn clean install package'
                //  Maybe add those tests here at some point. Need investigation and config.
                //              dir('webapps') {
                //                sh 'npm run e2e'
                //              }
              }
            }
          }
        }
      }
      stage('Run tests') {
        steps {
          echo "Hermes branch/commit: ${params.hermes_branch_or_commit}"        
          sh 'mkdir hermes'
          dir('hermes') {
            getRepoCode("https://github.com/vmwathena/hermes", params.hermes_branch_or_commit)          
            configFileProvider([configFile(fileId: '092fb643-feda-4b41-b7b0-31ff7617b0c9', targetLocation: 'resources/user_config.json')]) {
            }
            sh './main.py CoreVMTests'
            sh './main.py HelenAPITests'
            sh './main.py ExtendedRPCTests'
            sh './main.py RegressionTests'
          }
        }
      }
      // stage('Build docker images') {
      //   parallel {
      //     stage('Build helen docker image') {
      //       steps {
      //         script {
      //           dir('helen') {
      //             docker.build("helen:${env.BRANCH_NAME}")
      //           }
      //         }
      //       }
      //     }
      //     stage('Build athena docker image') {
      //       steps {
      //         script {
      //           dir('athena') {
      //             sh '''sed -i\'\' "s?genesis_block.*?genesis_block=/athena/resources/genesis.json?g" resources/athena1.config
      //             sed -i\'\' "s?genesis_block.*?genesis_block=/athena/resources/genesis.json?g" resources/athena2.config
      //             sed -i\'\' "s?genesis_block.*?genesis_block=/athena/resources/genesis.json?g" resources/athena3.config
      //             sed -i\'\' "s?genesis_block.*?genesis_block=/athena/resources/genesis.json?g" resources/athena4.config'''
      //             sh "./docker-build.sh ${env.BRANCH_NAME}"
      //           }
      //         }
      //       }
      //     }
      //   }
      // }
      // stage('Prepare docker compose') {
      //   steps {
      //     sh 'mkdir docker'
      //     dir('docker') {
      //       configFileProvider([configFile(fileId: '4bcb682e-fe63-4f12-944b-a8d1f93b81eb', targetLocation: 'docker-compose.yml')]) {
      //       }
      //       sh "sed -i'' 's/{{tag}}/${env.BRANCH_NAME}/g' docker-compose.yml"
      //     }
      //   }
      // }
      // RV 2018/09/20: When this runs, the docker/cockroachdb directory has files which Jenkins cannot delete.
      //                Need to investigate.
      // stage('Run the product in containers along with the tests') {
      //   steps {
      //     dir('hermes') {
      //       configFileProvider([configFile(fileId: 'd3dd9fd0-f578-4fae-a0dd-5a9ef81698cc', targetLocation: 'resources/user_config.json')]) {
      //       }
      //       sh "sed -i'' 's/{{build_root}}/..\\/docker\\//g' resources/user_config.json"
      //       sh './main.py CoreVMTests'

      //       // RV 2018/09/20: HelenAPITests in a Docker fail.
      //       //                Need toinvestigate.
      //       // sh './main.py HelenAPITests'

      //       sh './main.py ExtendedRPCTests'
      //       sh './main.py RegressionTests'
      //     }
      //   }
      // }
      // stage('Clean containers') {
      //   steps {
      //     dir('docker') {
      //       sh "docker-compose down"
      //     }
      //   }
      // }
    }// End stages
  }
}

// The user's parameter is top priority, and if it fails, let an exception be thrown.
// Next, get master.
// Next, try to get BRANCH_NAME.  If getting BRANCH_NAME fails, we are probably testing
// a branch that is in only in one or two of the repos.  That's fine.
void getRepoCode(repo_url, branch_or_commit){
  if (branch_or_commit.trim()){
    checkoutRepo(repo_url, branch_or_commit)
  }else{
    checkoutRepo(repo_url, "master")

    try {
      checkoutRepo(repo_url, env.BRANCH_NAME)
    } catch (Exception e) {
      echo "Branch ${env.BRANCH_NAME} for ${repo_url} not found"
    }
  }
}

// All that varies for each repo is the branch, so wrap this very large call.
void checkoutRepo(repo_url, branch_or_commit){
  checkout([$class: 'GitSCM', branches: [[name: branch_or_commit]], doGenerateSubmoduleConfigurations: false, extensions: [[$class: 'SubmoduleOption', disableSubmodules: false, parentCredentials: true, recursiveSubmodules: true, reference: '', trackingSubmodules: false]], submoduleCfg: [], userRemoteConfigs: [[credentialsId: '27bbd815-703c-4647-909b-836919db98ef', url: repo_url]]])
}