#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
from gevent import monkey
monkey.patch_all()

import time
import util.helper
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
        log.info("Startup for participant {}".format(self.ip))
        needs_wait = False
        reachable = False
        wait_time = 60

        while not reachable and wait_time > 0:
            statuses = util.helper.ssh_connect(self.ip,
                                               self.concord_username, self.concord_password,
                                               "docker ps -a --format '{{ .Names }}\t\t{{ .Status }}'")
            if statuses:
                reachable = True
            else:
                time.sleep(1)
                wait_time -= 1

        if not reachable:
            raise Exception("VM {} was not reachable".format(self.ip))

        statuses = statuses.split("\n")
        log.info("  Statuses:")
        for status in statuses:
            log.info("    {}".format(status))

            if "Exited" in status or "Restarting" in status:
                needs_wait = True

        if needs_wait:
            # Get list, or maybe this entire chunk of functionality, from central resource.
            names = ["telegraf", "jaeger-agent", "daml_ledger_api", "wavefront-proxy", "daml_index_db", "fluentd", "agent"]
            log.info("Waiting for new containers to come up")
            time.sleep(10)
            wait_time = 60
            all_up = False

            while not all_up and wait_time > 0:
                all_up = True
                statuses = util.helper.ssh_connect(self.ip,
                                                   self.concord_username, self.concord_password,
                                                   "docker ps -a --format '{{ .Names }} {{ .Status }}'")
                statuses = statuses.split("\n")
                log.info("  Statuses:")

                for name in names:
                    name_found = False

                    for status in statuses:
                        if status.split(" ")[0] == name:
                            name_found = True
                            break

                    if not name_found:
                        log.info("Docker container {} not found yet".format(name))
                        all_up = False
                        time.sleep(1)
                        wait_time -= 1
                        continue

                for status in statuses:
                    log.info("    {}".format(status))
                    if "Restarting" in status or "Exited" in status:
                        all_up = False
                        time.sleep(1)
                        wait_time -= 1
                        continue

            if wait_time <= 0:
                raise Exception("Cleaning VM failed")


    def pause_services(self, services):
        '''
        services: A list of the docker containers to pause.
        '''
        for svc in services:
            log.info("Pausing {}".format(svc))
            cmd = "docker pause {}".format(svc)
            util.helper.ssh_connect(self.ip, self.concord_username, self.concord_password, cmd)

            
    def unpause_services(self, services):
        '''
        services: A list of the docker containers to unpause.
        '''
        for svc in services:
            log.info("Unpausing {}".format(svc))
            cmd = "docker unpause {}".format(svc)
            util.helper.ssh_connect(self.ip, self.concord_username, self.concord_password, cmd)


    def stop_services(self, services):
        '''
        services: A list of the docker containers to stop.
        '''
        for svc in services:
            log.info("Stopping {}".format(svc))
            cmd = "docker stop {}".format(svc)
            util.helper.ssh_connect(self.ip, self.concord_username, self.concord_password, cmd)


    def start_services(self, services):
        '''
        services: A list of the docker containers to start.
        '''
        for svc in services:
            log.info("Starting {}".format(svc))
            cmd = "docker start  {}".format(svc)
            util.helper.ssh_connect(self.ip, self.concord_username, self.concord_password, cmd)

            
    def shutdown(self, timeout=10):
        '''
        Gets a VM handle before shutting down the VM, and returns the handle.
        The handle is needed to power it back on.
        '''
        vm = util.infra.findVMByInternalIP(self.ip)
        vm["entity"].PowerOffVM_Task()
        time.sleep(timeout)
        return vm


    def powerup(self, vm, timeout=20):
        '''
        The vm parameter is a vim handle returned by, for example, shutdown().
        '''
        log.info("Participant {} powering on".format(self.ip))
        vm["entity"].PowerOnVM_Task()
        time.sleep(timeout)


    def reboot(self, timeout=20):
        log.info("Participant {} shutting down".format(self.ip))
        vm = self.shutdown(timeout)
        self.powerup(vm, timeout)


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



