Boolean initializePython() {
  if (env.python_bin) { return false } // Already initialized

  env.python_bin = "/var/jenkins/workspace/venv_py37/bin"
  env.python = env.python_bin + "/python"

  sh '''
      # Adding websocket-client  0.56.0 to Artifactory: https://servicedesk.eng.vmware.com/servicedesk/customer/portal/12/INTSVC-549
      # When that is done, then start passing in -i <url to artifactory>
      . ${python_bin}/activate
      pip3 install -r hermes/requirements.txt
      deactivate
  '''
  return true
}

return this