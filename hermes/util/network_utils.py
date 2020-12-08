import docker
import util.hermes_logging

log = util.hermes_logging.getMainLogger()


class NetworkUtils(object):
    def __init__(self):
        self.docker_client = docker.from_env()

    def get_container_ip(self, container_name):
        con = self.docker_client.containers.get(container_name)
        return dict(list(con.attrs['NetworkSettings']['Networks'].values())[0])['IPAddress']

    def drop_all_packages_from_ip(self, target, source):
        source_ip = self.get_container_ip(source)
        target_con = self.docker_client.containers.get(target)
        target_con.exec_run("iptables -I INPUT -s " + source_ip + " -j DROP")

    def isolate_target(self, target, sources):
        for s in sources:
            self.drop_all_packages_from_ip(target, s)

    def flush_ip_tables(self, target):
        target_con = self.docker_client.containers.get(target)
        target_con.exec_run("iptables -F")
