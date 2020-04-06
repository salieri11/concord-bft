FROM photon:3.0
LABEL Description="Committer Tools"

COPY ./committer-tools /committer-tools
RUN tdnf update -y && tdnf install -y \
  vim \
  jq \
  gawk \
  sshpass \
  wget \
  && wget -O /usr/local/bin/yq \
    "https://github.com/mikefarah/yq/releases/download/3.2.1/yq_linux_amd64" \
  && chmod +x /usr/local/bin/yq \
  && rm -rf /var/cache/tdnf/*

WORKDIR /committer-tools
ENV PATH=$PATH:/committer-tools
CMD ["/bin/ls"]
