import groovy.json.JsonSlurperClassic

// The Performance team has requested we kick off a job on their server for every successful
// master run.  We will not wait for it or allow it to fail a run.
def startPerformanceRun(){
  withCredentials([
    usernamePassword(credentialsId: 'BLOCKCHAIN_SVC_ACCOUNT',
                    usernameVariable: 'username',
                    passwordVariable: 'password')
  ]){
    perfJson = readFile("vars/performance.json")
    perfObj = new JsonSlurperClassic().parseText(perfJson)
    env.maestroRecipients = perfObj.recipients
    env.maestroTests = perfObj.tests

    dir('performance/scripts') {
      echo("Starting a performance run in the Performance team's lab.")
      // Perf team needs this file at the base of our artifacts.
      env.performanceResultsHtmlDir = env.WORKSPACE
      env.performanceResultsHtmlFile = "performance.html"
      env.performanceResultsHtmlPath = env.performanceResultsHtmlDir + "/" + env.performanceResultsHtmlFile

      // Leave this one down in the performance/scripts directory.
      env.performanceResultsJsonDir = "performance/scripts"
      env.performanceResultsJsonFile = "performance.json"
      env.performanceResultsJsonPath = env.performanceResultsJsonDir + "/" + env.performanceResultsJsonFile

      status = sh(
        script: '''
          # Shhh
          set +x

          comments="Performance for build ${product_version}"
          cmd="maestroclient.py --username \"${username}\" --password '${password}'"
          cmd="${cmd} --recipients \"${maestroRecipients}\" --comments \\\"${comments}\\\""
          cmd="${cmd} --bcversion ${product_version} --htmlFile \\\"${performanceResultsHtmlPath}\\\""
          cmd="${cmd} --resultsFile \\\"${performanceResultsJsonFile}\\\""
          cmd="${cmd} --testList \\\"${maestroTests}\\\""
          eval "${python} ${cmd}"
        ''',
        returnStatus: true
      )

      echo("Exit status of launching maestroclient.py: " + status.toString())
      // We want to raise an exception, not have Jenkins fail the run.
      if (status != 0){
        throw new Exception("Maestro client error.")
      }
    }
    archiveArtifacts artifacts: env.performanceResultsJsonPath, allowEmptyArchive: true

    dir(env.WORKSPACE){
      // Has to be at the root in the artifact list in Jenkins.
      archiveArtifacts artifacts: env.performanceResultsHtmlFile, allowEmptyArchive: true
    }
  }
}

return this
