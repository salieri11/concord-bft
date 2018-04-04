/**
 * This interface defines the contract for any classes which wish to 
 * communicate with Athena.
 * 
 * Methods are defined for sending and receiving messages to and from Athena.
 */
package connections;

public interface IAthenaConnection {
   byte[] receive();

   boolean send(byte[] msg);
}
