node {
    // We use the "library" step here instead of the @Library directive at the
    // top of the file because we want to load the Builder class from whatever
    // the current branch is, and @Library requires that it be hard coded.
    def builder
    def buildLib

    if (params.shared_lib_branch_or_commit && params.shared_lib_branch_or_commit.trim()) {
       echo "Shared lib load: Explicitly given ${params.shared_lib_branch_or_commit}."
       buildLib = library("SharedJenkinsLib@${params.shared_lib_branch_or_commit}").org.vmware
    } else if (env.BRANCH_NAME && env.BRANCH_NAME.trim()) {
      try {
        // We need to know if env.BRANCH_NAME is in Hermes.  A test load of the library
        // will fail the Jenkins run, even if wrapped in a try/catch.  So use the checkout step.
        echo "Shared lib load: Found env.BRANCH_NAME is ${env.BRANCH_NAME}.  Seeing if that exists in Hermes."
        checkout([$class: 'GitSCM', branches: [[name: env.BRANCH_NAME]],
                  doGenerateSubmoduleConfigurations: false,
                  extensions: [[$class: 'SubmoduleOption', disableSubmodules: false,
                                parentCredentials: true, recursiveSubmodules: true,
                                reference: '', trackingSubmodules: false]],
                  submoduleCfg: [],
                  userRemoteConfigs: [[credentialsId: '27bbd815-703c-4647-909b-836919db98ef',
                                       url: "https://github.com/vmwathena/hermes"]]
                ])
        echo "${env.BRANCH_NAME} was found in Hermes, so the shared lib will be used from there."
        buildLib = library("SharedJenkinsLib@${env.BRANCH_NAME}").org.vmware
      } catch (Exception e) {
        echo "${env.BRANCH_NAME} not found in Hermes.  Using master."
        buildLib = library("SharedJenkinsLib@master").org.vmware
      }
    } else {
        echo "Neither env.BRANCH_NAME nor params.shared_lib_branch_or_commit exists. Using master for shared lib."
        buildLib = library("SharedJenkinsLib@master").org.vmware
    }

    builder = buildLib.Builder.new(this)
    builder.startBuild()
}
