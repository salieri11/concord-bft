# Fluentd Debian image
FROM fluent/fluentd:v1.7-debian-1
LABEL Description="Logging agent for Log Intelligence/Log Insight integration"

# Change user to root for installations & to access docker container logs
USER root

RUN buildDeps="sudo make gcc g++ libc-dev" \
 && apt-get update && apt-get install -y --no-install-recommends $buildDeps \
 && sudo gem install fluent-plugin-multi-format-parser \
 && sudo gem install fluent-plugin-concat \
 && sudo gem install fluent-plugin-out-http-ext \
 && sudo gem install fluent-plugin-vmware-loginsight \
 && sudo gem install fluent-plugin-record-modifier --no-document \
 && sudo gem install fluent-plugin-rewrite-tag-filter \
 && sudo gem sources --clear-all \
 && SUDO_FORCE_REMOVE=yes \
    apt-get purge -y --auto-remove \
                  -o APT::AutoRemove::RecommendsImportant=false \
                  $buildDeps \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /tmp/* /var/tmp/* /usr/lib/ruby/gems/*/cache/*.gem

# Copy configurations
COPY fluent-*.conf /fluentd/etc/
# Copy entrypoint file
COPY entrypoint.sh /bin/

# Set environment variables
ENV LOG_DESTINATION=LOG_INTELLIGENCE
ENV LINT_ENDPOINT_URL=lint_endpoint_url
ENV LINT_AUTHORIZATION_BEARER=lint_authorization_bearer
ENV LOG_INSIGHT_HOST=log_insight_host
ENV LOG_INSIGHT_PORT=9000
ENV LOG_INSIGHT_AGENT_ID=0
ENV CONSORTIUM_ID=consortium_id
ENV REPLICA_ID=replica_id
# DO NOT MODIFY FLUENT_CONF PATH
ENV FLUENT_CONF="/fluentd/etc/fluent.conf"

VOLUME ["/var/lib/docker/containers"]
