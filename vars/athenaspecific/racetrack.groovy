import groovy.json.JsonSlurperClassic

void racetrack(Map params){
  withCredentials([string(credentialsId: 'BUILDER_ACCOUNT_PASSWORD', variable: 'PASSWORD')]) {
    def action = params.action
    script {
      dir('blockchain/hermes') {
        try{
          if(action == "setBegin") {
            sh 'echo "${PASSWORD}" | sudo -SE "${python}" invoke.py racetrackSetBegin'
          } else if(action == "setEnd"){
            env.run_result = currentBuild.currentResult
            sh 'echo "${PASSWORD}" | sudo -SE "${python}" invoke.py racetrackSetEnd --param "${run_result}"'
          } else if(action == "reportFailure") {
            env.tmp_suite_name = params.suiteName ? params.suiteName : "_Pipeline"
            env.tmp_case_name = params.caseName ? params.caseName : "pipeline_unknown_case"
            env.tmp_case_description = params.caseDescription ? params.caseDescription : ""
            sh 'echo "${PASSWORD}" | sudo -SE "${python}" invoke.py racetrackCaseFailed --param "${tmp_suite_name}" "${tmp_case_name}" "${tmp_case_description}"'
          }
        }catch(Exception ex){
          echo(ex.toString())
        }
      }
    }
  }
}

return this
