// The Performance team has requested we kick off a job on their server for every successful
// master run.  We will not wait for it or allow it to fail a run.
def startOfficialPerformanceRun(perfObj){
  withCredentials([
    usernamePassword(credentialsId: 'BLOCKCHAIN_SVC_ACCOUNT',
                    usernameVariable: 'username',
                    passwordVariable: 'password')
  ]){
    env.maestroRecipients = perfObj.recipients

    dir('blockchain/performance/scripts') {
      echo("Starting a performance run in the Performance team's lab.")
      // Perf team needs this file at the base of our artifacts.
      env.performanceResultsHtmlDir = env.WORKSPACE
      env.performanceResultsHtmlFile = "performance.html"
      env.performanceResultsHtmlPath = env.performanceResultsHtmlDir + "/" + env.performanceResultsHtmlFile

      // Leave this one down in the performance/scripts directory.
      env.performanceResultsJsonDir = "blockchain/performance/scripts"
      env.performanceResultsJsonFile = "performance.json"
      env.performanceResultsJsonPath = env.performanceResultsJsonDir + "/" + env.performanceResultsJsonFile


      sh '''
        set +x

        # Never fail due to launching performance
        set +e

        comments="Performance for build ${product_version}"
        cmd="maestroclient.py --username \"${username}\" --password '${password}'"
        cmd="${cmd} --recipients \"${maestroRecipients}\" --comments \\\"${comments}\\\""
        cmd="${cmd} --bcversion ${product_version} --htmlFile \\\"${performanceResultsHtmlPath}\\\""
        cmd="${cmd} --resultsFile \\\"${performanceResultsJsonFile}\\\""
        eval "${python} ${cmd}"
        set -e
      '''
    }
    archiveArtifacts artifacts: env.performanceResultsJsonPath, allowEmptyArchive: true
    archiveArtifacts artifacts: env.performanceResultsHtmlFile, allowEmptyArchive: true
  }
}

return this