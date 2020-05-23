import groovy.json.JsonSlurperClassic

jenkinsbuilderlib = load "vars/util/jenkinsbuilderlib.groovy"

String getLatestTag(){
  if (env.latest_tag) { return env.latest_tag }

  def pythonlib = load "vars/util/pythonlib.groovy"
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
  jenkinsbuilderlib.retryCurl(curlCommand, true)

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

return this