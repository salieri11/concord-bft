import os
import subprocess
import socket

class ExternalBftClient:
    """
    This class communicates with ExternalClient class from concord/test/external_client/external_client_pool.cpp
    Check the comment at the start of the cpp file for details about the protocol they use.
    """
    def __init__(self):
      self.sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

    def _serialize_seq_num(self, seq_num):
      """
      8 bytes == 64bits; 
      Network byte order is always BigEndian
      Transfer unsigned int!
      """
      return (seq_num).to_bytes(8, byteorder='big', signed=False)

    def _send_rq(self, msg, flags, seq_num):
      if seq_num is not None and seq_num <= 0:
          raise RuntimeError(f"seq_num should be None or bigger than zero. Got {seq_num}.")
      
      seq_num_buf = self._serialize_seq_num(0) if seq_num is None else self._serialize_seq_num(seq_num)
      self.sock.sendto(flags + seq_num_buf + msg, ("127.0.0.1", 8989))
      resp = self.sock.recvfrom(64 * 1024)

      return resp[0]

    # From SimpleClient.hpp
    # enum ClientMsgFlag : uint8_t { EMPTY_FLAGS_REQ = 0x0, READ_ONLY_REQ = 0x1, PRE_PROCESS_REQ = 0x2 };

    async def write(self, msg, seq_num=None, pre_process=False):
      return self._send_rq(msg, b'\x02' if pre_process else b'\x00', seq_num)

    async def read(self, msg, seq_num=None):
      return self._send_rq(msg, b'\x01', seq_num)
