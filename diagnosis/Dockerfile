FROM alpine
LABEL Description="Committer Tools"

COPY ./committer-tools /committer-tools
RUN apk update && apk add \
  bash \
  vim \
  jq \
  gawk \
  sshpass \
  wget \
  openssh-client \
  && wget -O /usr/local/bin/yq \
    "https://github.com/mikefarah/yq/releases/download/3.2.1/yq_linux_amd64" \
  && chmod +x /usr/local/bin/yq \
  && rm -rf /var/cache/apk/*

WORKDIR /committer-tools
ENV PATH=$PATH:/committer-tools
CMD ["/bin/ls"]
