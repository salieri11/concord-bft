/**
 * this interface defines the contract for any classes that wish to communicate
 * with Athena methods are defined to send and receive messages to/from Athena
 * and checking the connection
 */
package com.vmware.blockchain.connections;

public interface IAthenaConnection {
   void close();

   boolean send(byte[] msg);

   byte[] receive();

   boolean check();
}
