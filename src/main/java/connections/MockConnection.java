package connections;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import com.vmware.athena.*;

import configurations.IConfiguration;

public class MockConnection implements IAthenaConnection {
   private static Athena.ProtocolResponse _protocolResponse
      = Athena.ProtocolResponse.newBuilder().setServerVersion(1).build();
   private IConfiguration _conf;

   public MockConnection(IConfiguration conf) {
      _conf = conf;
   }

   @Override
   public void close() {

   }

   @Override
   public boolean send(byte[] msg) {
      return true;
   }

   @Override
   /**
    * this method should be extended to remember last message sent and to return
    * corresponding response. Currently it returns hardcoded
    * AthenaProtocolResponse message
    */
   public byte[] receive() {
      byte[] data = _protocolResponse.toByteArray();
      int headerLength = _conf.getIntegerValue("ReceiveHeaderSizeBytes");
      byte[] bytes = ByteBuffer.allocate(headerLength + data.length)
                               .order(ByteOrder.LITTLE_ENDIAN)
                               .putShort((short) data.length)
                               .put(data)
                               .array();
      return bytes;
   }

   @Override
   public boolean check() {
      return true;
   }

}
