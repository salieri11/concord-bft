package connections;

public interface IAthenaConnection {
   void close();
   boolean send(byte[] msg);
   byte[] receive();
   boolean check();
}
