
# Fluentd image for VMware Log Intelligence
FROM fluent/fluentd:v1.3-debian
LABEL Description="Logging agent for Log Intelligence integration"

RUN fluent-gem install fluent-plugin-out-http-ext
RUN fluent-gem install fluent-plugin-multi-format-parser
RUN fluent-gem install fluent-plugin-concat
RUN fluent-gem install fluent-plugin-formatter_pretty_json
