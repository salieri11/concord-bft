package configurations;

public interface IConfiguration {
   int getIntegerValue(String key);
   String getStringValue(String key);
   long getLongValue(String key);
}
