node ('build-coordinator') {
    checkout scm

    def rootDir = pwd()
    def builder = load "${rootDir}/vars/builder.groovy"
    builder.runBuild()
}
