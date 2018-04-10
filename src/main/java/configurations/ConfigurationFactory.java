package configurations;

import javax.naming.OperationNotSupportedException;

public class ConfigurationFactory {
   public enum ConfigurationType {
      File,
      Test
   }

   public static IConfiguration getConfiguration(ConfigurationType type) {
      switch (type) {
      case File:
         return FileConfiguration.getInstance();
      case Test:
         return TestConfiguration.getInstance();
      default:
         throw new ExceptionInInitializerError("Unsupported conf type");
      }
   }
}
