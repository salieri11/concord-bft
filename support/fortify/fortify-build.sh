#!/bin/bash

usage () {

cat << EOF
usage: $0 options
       $0 -p pom.xml -f true
  OPTIONS:
     -h Show this message
     -p <path/pom.xml>; full path to parent pom.xml file e.g: '-p /pathtocode/parent/build/pom.xml'
     -i <Project Id>; This is a numeric id based on the projects that are listed using the -l option
     -f <true>; Perform Fortify Scan e.g: '-f true'
     -a <artifactId>; This is usually the values in first few lines of parent pom.xml
     -v <project version>; Project version from fortify
EOF
}

POMFILE=
SCAN=
WARN=
ARTIFACTID=
PROJECTID=
MVNARG=
VERSION=

while getopts ":h:p:s:f:l:a:i:m:v:" OPTION
do
     case $OPTION in
	h)
	  usage
	  exit 1
	  ;;
	p)
	  POMFILE=$OPTARG
	  ;;
	f)
	  SCAN=true
	  ;;
	a)
	  ARTIFACTID=$OPTARG
	  ;;
	m)
	  MVNARG=$OPTARG
	  ;;
	i)
	  PROJECTID=$OPTARG
	  ;;
        v)
          VERSION=$OPTARG
          ;;
	?)
	  usage
	  exit
	  ;;
     esac
done

if [ -z ${TCROOT+x} ]; then
	echo -e "TCROOT is unset; setting it to /build/toolchain"
	TCROOT=/build/toolchain
fi
echo -e "TCROOT is set to: ${TCROOT}"
## pander: to add fortify in path
export PATH=${TCROOT}/lin64/fortify-ssc-19.20/bin:$PATH

export JAVA_HOME=/usr/lib/jvm/java-11-oracle/
export MAVEN_HOME=$TCROOT/noarch/apache-maven-3.5.0
export PATH=$MAVEN_HOME/bin:$PATH

# Ensuring that sourceanalyzer is a valid command
if command -v sourceanalyzer > /dev/null; then
	echo -e "PASS: fortify bin found in \$PATH"
else
	echo -e "FAIL: Add /mnt/fortify/bin to path"
	echo -e "\t\$vi ~/.bashrc"
	echo -e "\texport PATH=\$PATH/mnt/fortify/bin/"
	exit
fi

# Ensuring that mvn is valid command
if command -v mvn > /dev/null; then
	echo "PASS: mvn found"
else
	echo "FAIL: mvn not found"
	exit
fi

# Ensuring that pom.xml file has the fortify plugin group

if [ "$POMFILE" ]; then
	if grep -q '<groupId>com.fortify.ps.maven.plugin</groupId>' $POMFILE &&
	   grep -q '<artifactId>sca-maven-plugin</artifactId>' $POMFILE &&
	   grep -q '<version>3.90</version>' $POMFILE ; then

		echo "PASS: parent pom.xml file is properly configured"
	else
		echo "FAIL: parent pom.xml file is not properly configured"
		echo -e "Add the following in pom.xml file"
		echo -e "<project xmlns=...."
		echo -e "\t<build>"
		echo -e "\t\t<plugins>"
		echo -e "\t\t\t<plugin> "
		echo -e "\t\t\t\t<groupId>com.fortify.ps.maven.plugin</groupId>"
		echo -e "\t\t\t\t<artifactId>sca-maven-plugin</artifactId>"
		echo -e "\t\t\t\t<version>3.90</version>"
		echo -e "\t\t\t</plugin>"
		echo -e "\t\t</plugins>"
		echo -e "\t</build>"
		echo -e "</project>"
		exit
	fi
else
	echo -e "WARN: Not checking the configuration of pom.xml"
	echo -e "WARN: Highly recommend running using -p <pom file> option"
	WARN=true
fi

if [ "$WARN" != true ]; then
	echo -e "*************************************************************************************"
	echo -e "All Settings Look Good"
	if [[ -z "$SCAN" ]]; then
		echo -e "Consider adding the scan flag now (-f true)"
	fi
	echo -e "*************************************************************************************"
fi

### To execute commands
execCmd () {
	msg=$1
	cmd=$2

	echo -e "INFO: Starting $msg"
	echo -e "CMD: Executing $cmd"
	$cmd
	if [ $? -ne 0 ] ; then
		echo -e "ERROR: $cmd FAILED."
		exit 1
	else
		echo -e "INFO: $msg Completed"
	fi
}

# Starting to execute Scan

if [ "$SCAN" ] && [ "$POMFILE" ]; then

	echo -e "Starting the SCA Scan"
	echo -e "Moving to directory `dirname "$POMFILE"`"
	cd `dirname "$POMFILE"`

	echo -e "Ensuring that we have enough memory for mvn"
    export MAVEN_OPTS="-XX:MetaspaceSize=1536m -XX:MaxMetaspaceSize=2048m -Xms2048m -Xmx2560m -XX:ReservedCodeCacheSize=768m"
    export SCA_VM_OPTS="-Xmx5120m"
    # echo -e $MAVEN_OPTS
	execCmd "clean install" "mvn clean install -DskipTests $MVNARG"

	execCmd "clean" "mvn clean -P fortify $MVNARG"

	execCmd "sca-clean" "mvn -Dfortify.sca.buildId=FortifyBuild -Dcom.fortify.sca.JdkVersion=11.0.3 $MVNARG sca:clean -P fortify"

	execCmd "package" "mvn package -DskipTests=true -P fortify -DskipPackage=true $MVNARG"

	execCmd "translate" "mvn -Dfortify.sca.buildId=FortifyBuild -Dfortify.sca.source.version=11.0.3 sca:translate -P fortify $MVNARG"

	# echo -e "Printing artifact id:" $ARTIFACTID
	if [[ -z "$ARTIFACTID" ]]; then
		ARTIFACTID=`sed '/parent/,/\parent/d' "$POMFILE" | awk '/\<artifactId\>/ {start=index($0,"<artifactId>")+12;end=index($0,"</artifactId>");value=substr($0,start,end-start);print value;exit}'`
	fi
	if [ "$ARTIFACTID" ]; then
		echo -e "Printing artifact id:" $ARTIFACTID
	else
		echo -e "FAIL: Could not find the artifact id., best to feed this manually using the -a option"
		exit
	fi

	execCmd "Scan Only, no Upload" "mvn -Dfortify.sca.verbose="true" -Dfortify.sca.buildId=FortifyBuild -Dfortify.sca.toplevel.artifactId="$ARTIFACTID" -Dfortify.sca.debug=true -Dfortify.sca.renderSources=true sca:scan -P fortify $MVNARG"

	if [ "$PROJECTID" ] && [ "$VERSION" ]; then
		execCmd "Upload fpr" "/build/toolchain/lin64/fortify-ssc-17.20/bin/fortifyclient uploadFPR -f target/${ARTIFACTID}*.fpr -project "$PROJECTID" -version "$VERSION" -authtoken "$AUTHTOKEN" -url https://fortify.eng.vmware.com:8180/ssc"
        else
                echo -e "NOTE: If you would like automatic upload add -i <projecid> and -v <version>"
                exit
	fi

        execCmd "Report Generation" "ReportGenerator -format pdf -f ${ARTIFACTID}-OWASP2007.pdf -source target/${ARTIFACTID}*.fpr -template OWASP2007.xml"

	cd $OLDPWD

else
	if [ "$SCAN" ]; then
		echo -e "Error: Failed to specify parent POM file. Try"
		echo -e "$" $0 "-p <path-to-parent-pom-dir/pom.xml> -f true"
	fi
fi
