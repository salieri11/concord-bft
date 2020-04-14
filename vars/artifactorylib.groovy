import groovy.json.JsonSlurperClassic

String getLatestTag(){
  if (env.latest_tag) { return env.latest_tag }

  def pythonlib = load "vars/pythonlib.groovy"
  pythonlib.initializePython()

  withCredentials([
    string(credentialsId: 'BUILDER_ACCOUNT_PASSWORD', variable: 'PASSWORD'),
    string(credentialsId: 'ARTIFACTORY_API_KEY', variable: 'ARTIFACTORY_API_KEY')
  ]) {
    script {
      dir('vars'){
        sh 'echo "${PASSWORD}" | sudo -SE "${python}" ../docker/get-build-info.py --artifactoryKey "${ARTIFACTORY_API_KEY}"'
        latestTagJsonText = readFile('latest_tag.json')
        latestTagJson = new JsonSlurperClassic().parseText(latestTagJsonText)
        env.latest_tag = latestTagJson.latest_tag
        return env.latest_tag
      }
    }
  }
}

return this