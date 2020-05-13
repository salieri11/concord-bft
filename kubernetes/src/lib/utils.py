
import os
import json
import time
import subprocess
import logging
import requests
import kubernetes
import docker
import hvac
import slack
from uuid import UUID
from socket import gethostbyname, gaierror
from logging.handlers import RotatingFileHandler
from config import common, kube, cdn

GIT_COMMIT_DIFF = ("%s/%s/%s/git/commits?until=%s&"
                    "since=%s&order=topo&ancestry_path=true")

class ProcessTimeoutException(BaseException):
    pass

def setup_logging(log_level=logging.INFO, log_file=None):
    """Setup stream and file loggers
        Args:
            log_level: default log level to display
            log_file: log as module.log if True
                      log file with the filename of log_file
        Return: log object
    """
    if log_file is None or log_file is True:
        logger_file = __file__
    else:
        logger_file = log_file
    logger_path = os.path.dirname(logger_file)
    logger_name = os.path.splitext(os.path.basename(logger_file))[0]
    if log_file is not None:
        log_file = os.path.join(logger_path, logger_name + ".log")
    log = logging.getLogger(logger_name)
    if not log.handlers:
        log.setLevel(logging.INFO)
        formatter = logging.Formatter(
            fmt='[%(asctime)s %(levelname)5s %(module)s:%(lineno)03d]'
                '  %(message)s',
            datefmt='%m/%d/%y %H:%M:%S')
        # Console handler
        ch = logging.StreamHandler()
        ch.setFormatter(formatter)
        ch.setLevel(log_level)
        log.addHandler(ch)
        # File handler
        if log_file:
            # Rotate log file every 1M
            fh = logging.handlers.RotatingFileHandler(filename=log_file,
                                                      mode='a',
                                                      maxBytes=1048576000,
                                                      backupCount=1)
            fh.setFormatter(formatter)
            fh.setLevel(logging.DEBUG)
            log.addHandler(fh)
    return log

def subproc(cmd, cwd=None, debug=None, logger=None, timeout=0):
    """
        Run a sub proc
        Args:
            cmd: command to run
            debug: prefix command with echo
            logger: logger to log
        Return: returncode, stdout + stderr
    """
    if debug:
        # split the commands
        cmd_list = cmd.split(';')
        cmd_list = ["echo %s" % cmd for cmd in cmd_list]
        cmd = "; ".join(cmd_list)
    if not cwd:
        cwd = os.environ.get('PWD')
    user = os.environ.get("LOGNAME")
    p = subprocess.Popen(cmd, shell=True, stdin=None, bufsize=-1,
                         stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                         cwd=cwd)
    start = time.time()
    while p.poll() is None:
        if timeout and start + timeout < time.time():
            p.kill()
            raise ProcessTimeoutException("Command %s timed out after %s "
                                          "seconds" % (cmd, timeout))
        time.sleep(1)
    rv = p.communicate()[0]
    rc = p.returncode
    if logger:
        logger.info("CMD (%s:%s) $ %s\n%s" % (user, cwd, cmd, rv))
    return rc, rv

def request_url(url, data_dict=None, header_dict=None, method=None):
    """Post data to url with method of GET, PUT, POST, DELETE
        Return (response code, response data)
    """
    if method is None:
        method = "GET"
    response = requests.request(method=method, url=url, data=data_dict,
                                headers=header_dict, timeout=60)
    rc = response.status_code
    rv = response.text
    if rv.startswith(")]}'"):
        rv = rv[4:]
    return rc, rv


def parse_request_json(url, data_dict=None,
                       header_dict=None, method="GET",
                       logger=None):
    rc, rv = request_url(
        url, data_dict=data_dict, header_dict=header_dict,
        method=method)
    if rc == 200:
        return json.loads(rv)
    else:
        raise Exception(rv)


def list_images_on_containerrepo(registry, repo, credentials, logger=None):
    """
        list images on repo
        Args:
            registry name (currently only bintray)
            credentials (username, password) as tuple
        Return: json of all images/artifacts on repo path
    """
    req = requests.get("%s/repos/vmware/%s/packages" % (registry, repo),
                         auth=credentials)
    if req.status_code != 200:
        if logger:
            logger.info("API request failed %s" % r.text)
        return None
    else:
        return json.loads(req.text)

def wait_for_dns_resolve(hostname, timeout=1800, logger=None):
    """
        Poll on a hostname till timeout expires (in seconds)
    """
    count = 0
    while count < 60:
        try:
            gethostbyname(hostname)
            if logger:
               logger.info("Hostname %s is successfully resolving" % hostname)
            return True
        except gaierror:
            if logger:
                logger.info("Hostname not resolving, polling in 30 seconds")
            time.sleep(30)
            count += 1
    if logger:
        logger.error("Failed to resolve %s within %s seconds" %
                     (hostname, timeout))
    return False


def checkout_branch(repopath, branch="master", logger=None):
    """
        Check out branch from git repo
    """
    subproc("cd %s && git checkout master" % repopath, logger=logger)
    subproc("cd %s && git pull --rebase" % repopath, logger=logger)
    if not branch.startswith("origin"):
        branch = "origin/%s" % branch
    subproc("cd %s && git checkout %s" % (repopath,branch), logger=logger)


def get_authenticated_hvac(endpoint):
    """
        Initialize and authenticate client handle to vault
    """
    if common.VAULT_KEY is None:
        raise Exception("Environment variable VAULT_KEY not set")
    client = hvac.Client(url=endpoint, token=common.VAULT_KEY)
    if client.is_authenticated() == True:
        return client


def get_vault_constants(path, logger=None):
    """
        Populate constants from vault for datacenter
    """
    client = get_authenticated_hvac(common.VAULT_ENDPOINT)
    if path in common.VAULT_KNOWN_KEYS:
        data = client.secrets.kv.v2.read_secret_version(
                        mount_point="kv", path=path)["data"]["data"]
        return data
    else:
        if logger is not None:
            logger.error("Vault path not found")
            return None


def get_auth_header(service, apitoken):
    """
        get auth header for csp service
    """
    key = 'oauthkey'
    uri = '/am/api/auth/api-tokens/authorize'
    headers = {'Content-Type':'application/json'}
    payload = {'refresh_token': apitoken}
    req = requests.post(f'{service}{uri}', headers=headers, params=payload)
    auth_header = req.json()['access_token']
    final_header = {'Content-Type':'application/json',
                    'csp-auth-token':auth_header}
    return final_header


def get_full_commitid(commitid, repo):
    """
        Given short commitid , get full git sha
    """
    client = get_authenticated_hvac(common.VAULT_ENDPOINT)
    gitdata = client.secrets.kv.v2.read_secret_version(
                            mount_point="kv", path="git")["data"]["data"]
    repoid = gitdata[repo]
    gitlab_token = gitdata["GITLAB_TOKEN"]
    url = ("%s/projects/%s/repository/commits/%s" %
                (cdn.GITLAB_API, repoid, commitid))
    data = parse_request_json(url,
                header_dict={'PRIVATE-TOKEN': gitlab_token})
    return data["id"]


def get_changelog(higher, lower, repo):
    """
        Get changelog from repo for the given commit range
        repo: git repository
        higher: newer commit
        lower: older commit
    """
    gitdata = get_vault_constants("git")
    # desperado api needs full commit sha to resolve commit
    l_commit = get_full_commitid(lower, repo)
    h_commit = get_full_commitid(higher, repo)
    commiturl = GIT_COMMIT_DIFF % (cdn.GIT_API,
                    cdn.GITLAB_DEFAULT_GROUP, repo, h_commit, l_commit)
    auth_token = gitdata["DESPERADO_AUTH_TOKEN"]
    gitlab_token = gitdata["GITLAB_TOKEN"]
    commitlist = parse_request_json(commiturl, header_dict={
        'Authorization': 'Token %s' % auth_token
        })
    msg_list = []
    for commit in commitlist['data']:
        commit_data = parse_request_json(commit['url'], header_dict={
            'PRIVATE-TOKEN': gitlab_token
            })
        msg_list.append("#############################")
        msg_list.append("Commit: %s" % commit_data['id'])
        msg_list.append("Commit date: %s" % commit_data['committed_date'])
        msg_list.append("Committer name: %s" % commit_data['committer_name'])
        msg_list.append("Committer email: %s" % commit_data['committer_email'])
        msg_list.append("Commit message: %s" % commit_data['message'])
        msg_list.append("#############################")
    return msg_list


def json_to_file(payload, filename, logger=None):
    try:
        with open(filename, 'w') as outfile:
            json.dump(data, outfile)
    except Exception as e:
        logger.exception("Error dumping json data")


def post_slack_channel(channel, text):
    data = get_vault_constants("slack")
    c_data = data[channel]
    client = slack.WebClient(c_data["apitoken"])
    client.chat_postMessage(
            channel=c_data["channelid"],
            text=text)


def get_kube_deployment_version(env, componentapp):
    kenv = kube.KUBE_CONFIGS[env]
    path = "'{.items[*].metadata.labels.version}'"
    rc, rv = subproc("kubectl config use-context %s" %
                kenv["context"])
    if rc != 0:
        return 1
    cmd = ("kubectl -n %s get deployments. -l app=%s -o jsonpath=%s"
           % (kenv["namespace"], componentapp, path))
    rc, rv = subproc(cmd)
    if rc == 0:
        return rv.decode()
    else:
        1

def get_default_concord(env, appname):
    kenv = kube.KUBE_CONFIGS[env]
    path = "'{.items[*].metadata.labels.defaultconcord}'"
    rc, rv = subproc("kubectl config use-context %s" %
                kenv["context"])
    if rc != 0:
        return 1
    cmd = ("kubectl -n %s get deployments. -l app=%s -o jsonpath=%s"
           % (kenv["namespace"], appname, path))
    rc, rv = subproc(cmd)
    if rc == 0:
        return rv.decode()
    else:
        return 1

def validate_uuid4(uuid_string):
    try:
        UUID(uuid_string, version=4)
    except ValueError:
        return False
    return True

def get_network_metadata(vms):
    """
        Strip out nat rule id and public ip from vm name
    """
    metadata = {}
    for vm in vms:
        metadata[vm] = "-".join(vm.split("-")[5:])
    return metadata