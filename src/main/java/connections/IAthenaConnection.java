package connections;

public interface IAthenaConnection {
   byte[] receive();
   boolean send(byte[] msg);
}
