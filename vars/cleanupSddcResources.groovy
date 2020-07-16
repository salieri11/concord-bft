import groovy.json.JsonOutput
import groovy.json.JsonSlurperClassic
import groovy.transform.Field
import hudson.util.Secret

def call(){

  pipeline {
    agent { label params.jenkins_node ? params.jenkins_node : "genericVM" }
    triggers {
      cron('H * * * *') // Every hour.
    }
    options{
      gitLabConnection('TheGitlabConnection')
    }
    stages {
      stage("Setup"){
        steps{
          script{
            try{
              cleanWs()

              // Try dealing with https://issues.jenkins-ci.org/browse/JENKINS-48300. Run failed with this error:
              // "JENKINS-48300: if on an extremely laggy filesystem, consider -Dorg.jenkinsci.plugins.durabletask.BourneShellScript.HEARTBEAT_CHECK_INTERVAL=86400"
              System.setProperty("org.jenkinsci.plugins.durabletask.BourneShellScript.HEARTBEAT_CHECK_INTERVAL", "86400")

            }catch(Exception ex){
              failRun(ex)
              throw ex
            }
          }
        }
      }

      stage("Start Cleanup Schedule"){
        steps {
          script {
            cleanUpSDDCs()
          }
        }
      }

    }// End stages
  }
}

// Called when it fails.
void failRun(Exception ex = null){
  if (ex != null){
    echo "The run has failed with an exception: " + ex.toString()
  }
}

void announceSDDCCleanupFailures(failures){
  // Don't mass notify with SDDC clean-up failure if the run was simply aborted.
  if (currentBuild.currentResult == "ABORTED") { return }

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

      python3 invoke.py slackPost --param blockchain-concierge "${cleanup_fail_msg}"
      deactivate
    '''
  }
}

// Clean all SDDCs.
// Store all failures in an array and report them in one message.
void cleanUpSDDCs(){
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

  // HermesTesting-1day-Retention clean up older than 24 hrs (1 day)
  for(sddc in sddcs){
    failure = cleanSDDC(sddc, "HermesTesting-1day-Retention", "24")
    if (failure){
      failures << failure
    }
  }

  if (failures.size() > 0){
    announceSDDCCleanupFailures(failures)
  }
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