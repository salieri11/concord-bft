FROM alpine
LABEL Description="Committer Tools"

COPY ./committer-tools /committer-tools

WORKDIR /committer-tools
ENV PATH=$PATH:/committer-tools
CMD ["/bin/ls"]
