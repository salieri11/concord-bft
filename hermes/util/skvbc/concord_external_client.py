import os
import subprocess
import socket

class ExternalBftClient:
    def __init__(self):
      self.sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

    def _send_rq(self, msg, flags):
      self.sock.sendto(flags + msg, ("127.0.0.1", 8989))
      resp = self.sock.recvfrom(64 * 1024)

      return resp[0]

    # From SimpleClient.hpp
    # enum ClientMsgFlag : uint8_t { EMPTY_FLAGS_REQ = 0x0, READ_ONLY_REQ = 0x1, PRE_PROCESS_REQ = 0x2 };

    async def write(self, msg, seq_num=None, pre_process=False):
      return self._send_rq(msg, b'\x02' if pre_process else b'\x00')

    async def read(self, msg, seq_num=None):
      return self._send_rq(msg, b'\x01')
