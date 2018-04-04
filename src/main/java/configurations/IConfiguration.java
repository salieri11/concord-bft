/**
 * This interface defines the contract for any classes which are used for 
 * reading external configurations.
 * 
 * Methods are defined for reading different kinds of configurations.
 */
package configurations;

public interface IConfiguration {
   int getIntegerValue(String key);

   String getStringValue(String key);

   long getLongValue(String key);
}
