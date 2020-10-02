#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import time
import util.helper
import util.blockchain_ops
import util.hermes_logging as hermes_logging_util
log = hermes_logging_util.getMainLogger()

class ParticipantPool:
    '''
    This is a pool of participant nodes.
    '''
    def __init__(self, groups, concord_username, concord_password):
        '''
        groups: {
          "group1": [array of participant node url objects],
          ...
        }
        '''
        if not groups:
            raise Exception("Groups of participant node URLs are required to create a ParticipantPool.  Received '{}'".format(groups))

        self.participants = []

        for group in groups:
            for p_url in groups[group]:
                self.participants.append(Participant(p_url, group, concord_username, concord_password))


    def __str__(self):
        s = "Participant nodes:"

        for p in self.participants:
            s += "\n  {}".format(p)

        return s


    def wait_for_startup(self):
        for p in self.participants:
            p.wait_for_startup()


class Participant:
    '''
    This is a participant node.
    '''
    def __init__(self, p_url, group_name, concord_username, concord_password):
        self.ip = p_url.hostname
        self.port = p_url.port
        self.group = group_name
        self.concord_username = concord_username
        self.concord_password = concord_password


    def __str__(self):
        return "Group: {}, ip: {}, port: {}".format(self.group, self.ip, self.port)


    def wait_for_startup(self):
        '''
        Make sure we are powered on and able to receive transactions.
        '''
        names = util.helper.getDefaultParticipantContainers()
        util.blockchain_ops.wait_for_docker_startup(self.ip, self.concord_username, self.concord_password, names)


    def pause_services(self, services):
        '''
        services: A list of the docker containers to pause.
        '''
        util.blockchain_ops.pause_services(self.ip, self.concord_username, self.concord_password, services)


    def unpause_services(self, services):
        '''
        services: A list of the docker containers to unpause.
        '''
        util.blockchain_ops.unpause_services(self.ip, self.concord_username, self.concord_password, services)


    def stop_services(self, services):
        '''
        services: A list of the docker containers to stop.
        '''
        util.blockchain_ops.stop_services(self.ip, self.concord_username, self.concord_password, services)


    def start_services(self, services):
        '''
        services: A list of the docker containers to start.
        '''
        util.blockchain_ops.start_services(self.ip, self.concord_username, self.concord_password, services)


    def shutdown(self, timeout=10):
        '''
        Gets a VM handle before shutting down the VM, and returns the handle.
        The handle is needed to power it back on.
        '''
        return util.blockchain_ops.shutdown(self.ip, timeout)


    def powerup(self, vm, timeout=20):
        '''
        The vm parameter is a vim handle returned by, for example, shutdown().
        '''
        util.blockchain_ops.powerup(vm, timeout)


    def reboot(self, timeout=20):
        util.blockchain_ops.reboot(self.ip, timeout)


    def maintain_vim_connection(self):
        '''
        Our vim connection can time out while doing things that take a long time.
        Try using it periodically to see if that keeps it up.
        It may throw an exception because the VM cannot be found
        '''
        try:
            log.info("Maintaining vim connection")
            vm = util.infra.findVMByInternalIP(self.ip)
        except Exception as e:
            pass


    def get_db_size(self):
        '''
        Return the DB size in MB.
        '''
        cmd = "docker ps"
        containers = util.helper.ssh_connect(self.ip, self.concord_username, self.concord_password, cmd)
        db_docker_id = None
        db_id = None
        mb = None

        cmd = "cat /config/daml-index-db/environment-vars"
        environment_vars = util.helper.ssh_connect(self.ip, self.concord_username, self.concord_password, cmd)
        db_id = "daml_ledger_api"

        for line in containers.split("\n"):
            if "daml_index_db" in line:
                fields = line.split()
                db_docker_id = fields[0]

        cmd = "docker exec -it {} /usr/bin/psql {} indexdb -c \"select pg_database_size('{}')/1024/1024;\"".format(db_docker_id, db_id, db_id)
        output = util.helper.ssh_connect(self.ip, self.concord_username, self.concord_password, cmd)
        log.info("get_db_size output: {}".format(output))
        for line in output.split("\n"):
            try:
                mb = int(line.strip())
                break
            except ValueError:
                pass

        return mb
