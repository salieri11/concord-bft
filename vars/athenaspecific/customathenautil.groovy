import groovy.json.JsonSlurperClassic

void preprocessForMainMR(){
  // Figure out which components have changed based on git diff
  // From this information, you can:
  //    1) choose to pull pre-built images, instead of building everything
  //    2) skip component-internal unit tests of components that didn't change
  // Output the changed components map to `blockchain/vars/components_affected.json`
  // Also, extract effective commits and authors affecting this MR
  withCredentials([string(credentialsId: 'BUILDER_ACCOUNT_PASSWORD', variable: 'PASSWORD')]) {
    script {
      dir('blockchain/vars') {
        env.python = "/var/jenkins/workspace/venv_py37/bin/python"
        sh '''
          echo "${PASSWORD}" | sudo -SE "${python}" util/get-changed-paths.py > components_affected.log 2>&1
          cat components_affected.log
          echo "${PASSWORD}" | sudo -SE "${python}" util/get-commits-blame.py
          echo "${PASSWORD}" | sudo -SE "${python}" get-component-suites-and-builds.py
        '''
      }
    }
  }
}

void collectArtifacts(){
  saveTimeEvent("Gather artifacts", "Start")

  def logMap = [
          "tests": [
                  "base": "**/testLogs/**/*.",
                  "types": ["log", "csv", "txt", "json", "html", "png", "gz", "properties"]
          ],
          "builds": [
                  "base": "**/blockchain/**/*.",
                  "types": ["log", "json", "properties", "sql"]
          ]
  ]

  // Exclude 3rd party stuff; See BC-1858
  // node_modules; avoid bundling 3000+ package.json files
  def excludeMap = [
          "node_modules": [
                  "pattern": "**/node_modules/**",
          ]
  ]

  excludedPaths = ""
  for (k in excludeMap.keySet()){
    if (excludedPaths != "") excludedPaths += ","
    excludedPaths += excludeMap[k]["pattern"]
  }

  // Iterate through the keys, *not* the map, because of Jenkins.
  for (k in logMap.keySet()){
    paths = ""
    for (logType in logMap[k]["types"]){
      if (paths != "") paths += ","
      paths += logMap[k]["base"] + logType
    }
    archiveArtifacts artifacts: paths, excludes: excludedPaths, allowEmptyArchive: true
  }

  archiveArtifacts artifacts: "**/summary/**", allowEmptyArchive: true

  saveTimeEvent("Gather artifacts", "End")

  // And grab the time file one more time so we can know how long gathering artifacts takes.
  archiveArtifacts artifacts: env.eventsFile, allowEmptyArchive: false
}



// The version is: major.minor.patch.build
// The first three are read from build_info data file.
// The fourth digit is the Jenkins build number for
// the current run.
void setProductVersion(){
  raw_data = readFile("vars/build_info.json")
  version_object = new JsonSlurperClassic().parseText(raw_data)

  env.major_minor_patch = version_object["build_numbers"]["major"] + "." +
                          version_object["build_numbers"]["minor"] + "." +
                          version_object["build_numbers"]["patch"]
  env.product_version = env.major_minor_patch + "." + env.BUILD_NUMBER
}

// Given a host to connect to, use ssh-keygen to see if we have
// its ssh key in known_hosts. If not, use ssh-keyscan to fetch it,
// then verify with ssh-keygen.
void handleKnownHosts(host){
  // By setting returnStatus, we will get an exit code instead of
  // having the entire Jenkins run fail.
  status = sh (
          script: "ssh-keygen -F " + host,
          returnStatus: true
  )

  if(status != 0){
    // ssh-keyscan throws a nice error; let it bubble up.
    sh (
            script: "ssh-keyscan -H " + host + " >> ~/.ssh/known_hosts",
            )

    status = sh (
            script: "ssh-keygen -F " + host,
            returnStatus: true
    )

    if(status != 0){
      error("Unable to retrieve the ssh key for " + host)
    }
  }
}

void saveTimeEvent(stage, event){
  try {
    sh(script: "python3 \"${eventsRecorder}\" record_event '" + stage + "' '" + event + "' \"${eventsFullPath}\"")
  } catch(Exception ex) {
    echo "Save time event has failed: " + ex.toString()
  }
}

return this
