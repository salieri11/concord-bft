# Fluentd Debian image
FROM fluent/fluentd:v1.3-debian
LABEL Description="Logging agent for Log Intelligence/Log Insight integration"

RUN buildDeps="sudo make gcc g++ libc-dev ruby-dev" \
 && apt-get update \
 && apt-get install -y --no-install-recommends $buildDeps

RUN sudo gem install fluent-plugin-multi-format-parser \
 && sudo gem install fluent-plugin-concat \
 && sudo gem install fluent-plugin-formatter_pretty_json \
 && sudo gem install fluent-plugin-out-http-ext \
 && sudo gem install fluent-plugin-vmware-loginsight

RUN sudo gem sources --clear-all \
 && SUDO_FORCE_REMOVE=yes \
    apt-get purge -y --auto-remove \
                  -o APT::AutoRemove::RecommendsImportant=false \
                  $buildDeps \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /tmp/* /var/tmp/* /usr/lib/ruby/gems/*/cache/*.gem

# Set environment variables
ENV FLUENT_CONF="/fluentd/etc/fluent.conf"

# Copy configurations
COPY fluent-*.conf /fluentd/etc/
COPY entrypoint.sh /bin/

RUN ["chmod", "+x", "/bin/entrypoint.sh"]
