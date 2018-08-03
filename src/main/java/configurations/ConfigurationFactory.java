package configurations;

import java.io.FileNotFoundException;
import java.io.IOException;

public class ConfigurationFactory {

   private static String configFile = "application.properties";
   private static FileConfiguration fileConfiguration = null;
   private static TestConfiguration testConfiguration = null;

   public static void init() throws
           IOException {
      if (fileConfiguration == null && testConfiguration == null) {
         fileConfiguration = new FileConfiguration(configFile);
         testConfiguration = new TestConfiguration();
      }
   }

   public static void init(String configFile) throws IOException {
      ConfigurationFactory.configFile = configFile;
      init();
   }

   public static IConfiguration getConfiguration(ConfigurationType type) {
      if (fileConfiguration == null)
         throw new ExceptionInInitializerError("ConfigurationFactory "
            + "initialization not done. init must be called before calling"
            + " getConfiguration.");
      switch (type) {
      case File:
         return fileConfiguration;
      case Test:
         return testConfiguration;
      default:
         throw new ExceptionInInitializerError("Unsupported conf type");
      }
   }

   public enum ConfigurationType {
      File,
      Test
   }
}
