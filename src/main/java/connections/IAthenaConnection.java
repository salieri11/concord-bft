package connections;

public interface IAthenaConnection {
	byte[] readMessage();
	boolean sendMessage(byte[] msg);
}
