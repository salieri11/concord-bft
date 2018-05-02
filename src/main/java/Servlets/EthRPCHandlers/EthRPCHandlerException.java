/**
 * This class serves as a common exception type thrown by all EthRPC handlers.
 */
package Servlets.EthRPCHandlers;

public class EthRPCHandlerException extends Exception {
   private static final long serialVersionUID = 1L;

   public EthRPCHandlerException(String message) {
      super(message);
   }
}
