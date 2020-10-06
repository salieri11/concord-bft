#!/bin/bash

set -e

DATE_CMD="date --iso-8601=seconds | awk -F'+' '{print \$1}'"
function log {
  echo $(eval $DATE_CMD) $@
}

log $0 $@

chown concord /concord/rocksdbdata
chown concord /concord/config-generated
chown concord /concord/log
chown concord /concord/cores

# If we have a custom rocksdb configuration file in config-public, then copy it to the rocksdb folder
if [ -f /concord/config-public/rocksdb_default_conf.ini ]; then
  cp /concord/config-public/rocksdb_default_conf.ini /concord/rocksdbdata/OPTIONS_DEFAULT.ini
fi

# Concord can be initialized with a monolithic configuration file i.e. concord.config.
# Or with a set of three files that are being merged to a monolithic configuration on start up.
# For backward compatibility the monolithic file get precedence.
# The application configuration can be generated dynamically if an input file exists.
if [ "$#" -eq 0  ]; then
  sysctl kernel.core_pattern=/concord/cores/core.%e.%h.%s.%t >/dev/null
  ulimit -c unlimited

  # If uniform file exists, start with it.
  if [ -f "/concord/config-local/concord.config" ]; then
    log "Using monolithic configuration file"
    exec gosu concord /concord/concord -c /concord/config-local/concord.config
    exit
  fi

  # Split configuration - application, deployment and secrets.
  # Generate applicaiton configuration if input file exists
  APP_INPUT=/concord/config-local/appconf_input.yaml
  if [ -f $APP_INPUT ]; then
    log "Generating application configurarion from $APP_INPUT"
    /concord/conc_genconfig  --configuration-type application --output-name app --configuration-input $APP_INPUT

    #E.L check if file created and if not user should know.
    if [ ! -f /concord/app1.config ]; then
      log "Failed to generate application configuration from $APP_INPUT"
      exit
    fi
    cp -f /concord/app1.config /concord/config-local/application.config
  fi

  # Give precedence to an application configuration that was dynamically generated
  APP_CONF=/concord/config/application.config
  if [ -f "/concord/config-local/application.config" ]; then
    APP_CONF=/concord/config-local/application.config
  fi

  DEPLOYMENT_CONF=/concord/config-local/deployment.config
  SECRETS=/concord/config-local/secrets.config
  log "Split file configuration {-a $APP_CONF -d $DEPLOYMENT_CONF -s $SECRETS}"
  exec gosu concord /concord/concord -a $APP_CONF -d $DEPLOYMENT_CONF -s $SECRETS
elif [ "$1" = 'debug' ]; then
	exec gdb --args /concord/concord -a /concord/config-local/application.config -d /concord/config-local/deployment.config -s /concord/config-local/secrets.config
else
  exec "$@"
fi
