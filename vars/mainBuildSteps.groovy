def call(){
  pipeline {
    agent any
    tools {
        nodejs 'Node 8.9.1'
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
                sh 'cp -ar ~/workspace/googletest/* .'
              }
            }
          }
          stage('Copy evmjit') {
            steps() {
              sh 'mkdir evmjit'
              dir('evmjit') {
                sh 'cp -ar ~/workspace/evmjit/* .'
              }
            }
          }
          stage('Copy etherium tests') {
            steps() {
              sh 'mkdir ethereum_tests'
              dir('ethereum_tests') {
                sh 'cp -ar ~/workspace/ethereum_tests/* .'
              }
            }
          }
        }
      }
      stage('test email') {
        steps() {
          script {
            emailext (
              subject: "STARTED: Job '${env.JOB_NAME} [${env.BUILD_NUMBER}]'",
              body: """<p>STARTED: Job '${env.JOB_NAME} [${env.BUILD_NUMBER}]':</p>
                <p>Check console output at "<a href="${env.BUILD_URL}">${env.JOB_NAME} [${env.BUILD_NUMBER}]</a>"</p>""",
              recipientProviders: [[$class: 'DevelopersRecipientProvider']],
              to: "rvollmar@vmware.com"
            )
          }
        }
      }
      stage('Build products') {
        parallel {
          stage('Build Athena') {
            steps {
              sh 'mkdir athena'
              dir('athena') {
                checkout([$class: 'GitSCM', branches: [[name: "master"]], doGenerateSubmoduleConfigurations: false, extensions: [[$class: 'SubmoduleOption', disableSubmodules: false, parentCredentials: true, recursiveSubmodules: true, reference: '', trackingSubmodules: false]], submoduleCfg: [], userRemoteConfigs: [[credentialsId: '1a673da6-b7cf-46e9-9330-123797932588', url: 'https://github.com/vmwathena/athena']]])
                script {
                  try {
                    checkout([$class: 'GitSCM', branches: [[name: "${env.BRANCH_NAME}"]], doGenerateSubmoduleConfigurations: false, extensions: [[$class: 'SubmoduleOption', disableSubmodules: false, parentCredentials: true, recursiveSubmodules: true, reference: '', trackingSubmodules: false]], submoduleCfg: [], userRemoteConfigs: [[credentialsId: '1a673da6-b7cf-46e9-9330-123797932588', url: 'https://github.com/vmwathena/athena']]])
                  } catch (Exception e) {
                    echo "Branch ${env.BRANCH_NAME} for Athena not found"
                  }
                }
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
              sh 'mkdir helen'
              dir('helen') {
                checkout([$class: 'GitSCM', branches: [[name: "master"]], doGenerateSubmoduleConfigurations: false, extensions: [[$class: 'SubmoduleOption', disableSubmodules: false, parentCredentials: true, recursiveSubmodules: true, reference: '', trackingSubmodules: false]], submoduleCfg: [], userRemoteConfigs: [[credentialsId: '1a673da6-b7cf-46e9-9330-123797932588', url: 'https://github.com/vmwathena/helen']]])
                script {
                  try {
                    checkout([$class: 'GitSCM', branches: [[name: "${env.BRANCH_NAME}"]], doGenerateSubmoduleConfigurations: false, extensions: [[$class: 'SubmoduleOption', disableSubmodules: false, parentCredentials: true, recursiveSubmodules: true, reference: '', trackingSubmodules: false]], submoduleCfg: [], userRemoteConfigs: [[credentialsId: '1a673da6-b7cf-46e9-9330-123797932588', url: 'https://github.com/vmwathena/helen']]])
                  } catch (Exception e) {
                    echo "Branch ${env.BRANCH_NAME} for Helen not found"
                  }
                }
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
          sh 'mkdir hermes'
          dir('hermes') {
            checkout([$class: 'GitSCM', branches: [[name: "master"]], doGenerateSubmoduleConfigurations: false, extensions: [[$class: 'SubmoduleOption', disableSubmodules: false, parentCredentials: true, recursiveSubmodules: true, reference: '', trackingSubmodules: false]], submoduleCfg: [], userRemoteConfigs: [[credentialsId: '1a673da6-b7cf-46e9-9330-123797932588', url: 'https://github.com/vmwathena/hermes']]])
            configFileProvider([configFile(fileId: '092fb643-feda-4b41-b7b0-31ff7617b0c9', targetLocation: 'resources/user_config.json')]) {
            }
            sh './main.py CoreVMTests'
            sh './main.py HelenAPITests'
            sh './main.py ExtendedRPCTests'
            sh './main.py RegressionTests'
          }
        }
      }
      stage('Build docker images') {
        parallel {
          stage('Build helen docker image') {
            steps {
              script {
                dir('helen') {
                  docker.build("helen:${env.BRANCH_NAME}")
                }
              }
            }
          }
          stage('Build athena docker image') {
            steps {
              script {
                dir('athena') {
                  sh '''sed -i\'\' "s?genesis_block.*?genesis_block=/athena/resources/genesis.json?g" resources/athena1.config
                  sed -i\'\' "s?genesis_block.*?genesis_block=/athena/resources/genesis.json?g" resources/athena2.config
                  sed -i\'\' "s?genesis_block.*?genesis_block=/athena/resources/genesis.json?g" resources/athena3.config
                  sed -i\'\' "s?genesis_block.*?genesis_block=/athena/resources/genesis.json?g" resources/athena4.config'''
                  sh "./docker-build.sh ${env.BRANCH_NAME}"
                }
              }
            }
          }
        }
      }
      stage('Prepare docker compose') {
        steps {
          sh 'mkdir docker'
          dir('docker') {
            configFileProvider([configFile(fileId: '4bcb682e-fe63-4f12-944b-a8d1f93b81eb', targetLocation: 'docker-compose.yml')]) {
            }
            sh "sed -i'' 's/{{tag}}/${env.BRANCH_NAME}/g' docker-compose.yml"
          }
        }
      }
      stage('Run the product in containers along with the tests') {
        steps {
          dir('hermes') {
            configFileProvider([configFile(fileId: 'd3dd9fd0-f578-4fae-a0dd-5a9ef81698cc', targetLocation: 'resources/user_config.json')]) {
            }
            sh "sed -i'' 's/{{build_root}}/..\\/docker\\//g' resources/user_config.json"
            sh './main.py CoreVMTests'
            sh './main.py HelenAPITests'
            sh './main.py ExtendedRPCTests'
            sh './main.py RegressionTests'
          }
        }
      }
      stage('Clean containers') {
        steps {
          dir('docker') {
            sh "docker-compose down"
          }
        }
      }
    }
  }
}
