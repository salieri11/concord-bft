# Helper file with common utility methods

import logging

log = logging.getLogger(__name__)
docker_env_file = ".env"


def get_docker_env(env_key):
   '''
   Helper method to read docker .env file and return the value for a
   key being passed
   :param env_key: Key from the .env file
   :return: value
   '''
   env = {}
   with open(docker_env_file) as file:
      for line in file:
         key, val = line.partition("=")[::2]
         env[key.strip()] = val.strip()

   if env_key in env.keys():
      return env[env_key]
   else:
      log.error("No entry found for key {}".format(env_key))
      return None
