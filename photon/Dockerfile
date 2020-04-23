ARG VERSION=20200306
FROM photonos-docker-local.artifactory.eng.vmware.com/photon3:${VERSION}

ARG build
ARG commitId

LABEL Build=$build CommitID=$commitId

# Configure pip to install from artifactory
RUN echo -e "[global]\nindex-url = https://build-artifactory.eng.vmware.com/artifactory/api/pypi/pypi-virtual/simple" > /etc/pip.conf

# Base image for all docker images.  Install packages used by most images
# - python
# - python-pip
# - openjdk
# - sed
# Also adding some useful tools (dig, lsof, ps, etc)
# - binutils
# - bindutils
# - procps-ng
# - lsof
# - traceroute
# - gawk - needed by OSS/TP client utilities
# - unzip
# - which
RUN set -x && \
    tdnf -y update && \
#    tdnf upgrade tdnf --refresh && \
    tdnf -y remove toybox && \
    tdnf -y install python3 python-pip sed jq binutils bindutils procps-ng lsof traceroute gawk rpm unzip which gzip tar && \
    tdnf -y clean all && \
    rm -r /var/cache/tdnf/* && \
    pip install --upgrade pip && \
    pip install docutils --upgrade

RUN set -x && \
    curl https://build-artifactory.eng.vmware.com/openjdk-generic/jdk11/11.0.7/zulu11.39.16-sa-jdk11.0.7-linux_x64.tar.gz -Ss -o zulujdk.tar.gz -m 360 && \
    tar -zvxf zulujdk.tar.gz && \
    mv zulu11.39.16-sa-jdk11.0.7-linux_x64 /usr/local/ && \
    rm zulujdk.tar.gz && \
    for i in `ls /usr/local/zulu11.39.16-sa-jdk11.0.7-linux_x64/bin/`; do [ -x "/usr/local/zulu11.39.16-sa-jdk11.0.7-linux_x64/bin/$i" ] && ln -s /usr/local/zulu11.39.16-sa-jdk11.0.7-linux_x64/bin/$i /usr/bin/$i; done

# Setup service user, group and service dirs
ENV SVC_HOME=/opt/vmware
RUN mkdir -p $SVC_HOME && \
    groupadd -g 1000 vmware && \
    useradd -s /sbin/nologin -d $SVC_HOME -g vmware -u 1000 -c 'Service User' vmware && \
    chown -R vmware:vmware $SVC_HOME

# enable unlimited key length
# Enabled by default for jdk11
#RUN set -x && export JAVA_HOME=$(dirname "$(dirname "$(readlink -f "$(which javac || which java)")")") && echo "crypto.policy=unlimited" >> $JAVA_HOME/jre/lib/security/java.security

RUN sed -i "/jdk.tls.disabledAlgorithms=/ s/=.*/=TLSv1.3, SSLv3, RC4, MD5withRSA, DH keySize < 1024, EC keySize < 224, DES40_CBC, RC4_40, 3DES_EDE_CBC/" $(readlink -f /usr/bin/java | sed "s:bin/java::")/conf/security/java.security

RUN echo $(which javac || which java)
RUN echo "$(readlink -f "$(which javac || which java)")"
RUN echo $(dirname "$(dirname "$(readlink -f "$(which javac || which java)")")")