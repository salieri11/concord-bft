import groovy.json.JsonSlurperClassic

jenkinsbuilderlib = load "vars/util/jenkinsbuilderlib.groovy"
artifactorylib = load "vars/util/artifactorylib.groovy"

// Given a repo and tag, pushes a docker image to whatever repo we
// are currently logged into (set up by the caller).  If tagAsLatest
// is set to true, that image will be re-tagged as latest and pushed
// again.
void pushDockerImage(repo, tag, tagAsLatest){
  // TESTING CODE ONLY
  // Push to a test repo in artifactory instead of the real one when testing push.
  // if (repo.contains("athena-docker-local")){
  //     old_repo = repo
  //     repo = repo.replace("athena-docker-local.artifactory.eng.vmware.com", "athena-docker-local.artifactory.eng.vmware.com/test")
  //     command = "docker tag ${old_repo}:${tag} ${repo}:${tag}"
  //     retryCommand(command, true)
  // }

  if (repo.contains(env.internal_repo_name)){
    // Re-pushing to artifactory will trigger an error.
    component = repo.split("eng.vmware.com")[1]
    apiLookupString = env.internal_repo_name + component + "/" + tag

    if (artifactorylib.existsInArtifactory(apiLookupString)){
      return
    }
  }

  jenkinsbuilderlib.retryCommand("docker push ${repo}:${tag}", true)

  if(tagAsLatest){
    command = "docker tag ${repo}:${tag} ${repo}:latest && docker push ${repo}:latest"
    jenkinsbuilderlib.retryCommand(command, true)
  }
}

// Tag orig_repo:docker_tag as new_repo:docker_tag and push new_repo:docker_tag to dockerhub.
void tagAndPushDockerImage(orig_repo, new_repo, docker_tag) {
  jenkinsbuilderlib.retryCommand("docker tag ${orig_repo}:${docker_tag} ${new_repo}:${docker_tag}", true)
  pushDockerImage(new_repo, docker_tag, false)
}

// Remove all containers.
// Set returnStatus to true so that a build does not fail when there are no
// containers.  That should never happen anyway, but just in case.
void removeContainers(){
  echo "Removing docker containers"
  sh(script: '''
    set +e
    docker rm -f $(docker ps -aq) > /dev/null
  ''', returnStatus: true)
}

// Remove all images.
void removeImages(){
  echo "Removing docker images"
  sh(script: '''
    set +e
    docker image rm -f $(docker images -q) > /dev/null
  ''', returnStatus: true)
}

return this
