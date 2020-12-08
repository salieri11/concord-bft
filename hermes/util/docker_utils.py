import docker


class DockerUtils(object):
    def __init__(self, container_names):
        self.containers = []
        self.client = docker.from_env()
        for c in container_names:
            self.containers += [self.client.containers.get(c)]

    def get_client(self):
        return self.client

    def exec_cmd(self, id, cmd):
        con = self.containers[id]
        return con.exec_run(cmd)[1]

    def stop_container(self, id):
        con = self.containers[id]
        con.stop()

    def start_container(self, id):
        con = self.containers[id]
        con.start()

    def restart_container(self, id):
        con = self.containers[id]
        con.restart()

concord_containers = ["docker_concord1_1", "docker_concord2_1", "docker_concord3_1", "docker_concord4_1"]
operator_containers = ["docker_operator_1"]
