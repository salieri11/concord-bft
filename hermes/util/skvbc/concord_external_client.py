import os
import subprocess
import socket

class ExternalBftClient:
    """
    Targets multiple SKVBC API endpoints. If a request fails, rotates its current endpoint.
    """
    def __init__(self, f, c, n):
      self.sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

    def _send_rq(self, msg):
      self.sock.sendto(msg, ("127.0.0.1", 8989))
      resp = self.sock.recvfrom(64 * 1024)

      return resp[0]

    async def write(self, msg, seq_num=None):
      return self._send_rq(msg)

    async def read(self, msg, seq_num=None):
      return self._send_rq(msg)