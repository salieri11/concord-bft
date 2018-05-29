package configurations;

public class ConfigurationFactory {

   private static String configFile = "config.properties";
   private static FileConfiguration fileConfiguration;
   private static TestConfiguration testConfiguration;

   public static void init() {
      fileConfiguration = new FileConfiguration(configFile);
      testConfiguration = new TestConfiguration();
   }

   public static void init(String configFile) {
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
