node{
  checkout scm

  def rootDir = pwd()
  def builder = load "${rootDir}/vars/builder.groovy"
  builder.runBuild()
}
