import groovy.json.JsonSlurperClassic

jenkinsbuilderlib = load "vars/util/jenkinsbuilderlib.groovy"
artifactorylib = load "vars/util/artifactorylib.groovy"

// Given a repo and tag, pushes a docker image to whatever repo we
// are currently logged into (set up by the caller).  If tagAsLatest
// is set to true, that image will be re-tagged as latest and pushed
// again.
// :param: context, string description of category why the image is pushed (e.g. "Master-Artifact")
void pushDockerImage(context, repo, tag, tagAsLatest=false){
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

  component_name = repo.split("/").last()
  push_log_file = "${WORKSPACE}/blockchain/z_docker_push_${context}_${component_name}.log"
  jenkinsbuilderlib.retryCommand("docker push ${repo}:${tag} > '${push_log_file}'", true)

  latest_tag_push_log_file = "${WORKSPACE}/blockchain/z_docker_push_${context}_${component_name}_as_latest.log"
  if(tagAsLatest){
    command = "docker tag ${repo}:${tag} ${repo}:latest && docker push ${repo}:latest > '${latest_tag_push_log_file}'"
    jenkinsbuilderlib.retryCommand(command, true)
  }
}

// Tag orig_repo:docker_tag as new_repo:docker_tag and push new_repo:docker_tag to dockerhub.
// :param: context, string description of category why the image is pushed (e.g. "Master-Artifact")
void tagAndPushDockerImage(context, orig_repo, new_repo, docker_tag, new_docker_tag='', ignore_tag_issues=false) {
  if (!new_docker_tag) { new_docker_tag = docker_tag } // if not supplied, only change repo; keep the same tag
  if (orig_repo == new_repo && docker_tag == new_docker_tag) {
    echo("Source and destination are exactly the same; no need to tag or push.")
    return
  }
  result = jenkinsbuilderlib.retryCommand("docker tag ${orig_repo}:${docker_tag} ${new_repo}:${new_docker_tag}", !ignore_tag_issues)
  if (!result && ignore_tag_issues) {
    echo("Tagging operation failed and ignore flag was set; will skip pushing.")
    return;
  }
  pushDockerImage(context, new_repo, new_docker_tag, false)
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
