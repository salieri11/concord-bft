
# Fluentd image for VMware Log Intelligence
FROM fluent/fluentd:v1.3.2-debian
LABEL Description="Logging agent for Log Intelligence integration"

RUN fluent-gem install fluent-plugin-out-http-ext
RUN fluent-gem install fluent-plugin-multi-format-parser
