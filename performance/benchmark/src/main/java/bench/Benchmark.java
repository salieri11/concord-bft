package bench;

public class Benchmark {

  private SimpleConfig simpleConfig;
  private AdvancedConfig advancedConfig;

  public AdvancedConfig getAdvancedConfig() {
    return advancedConfig;
  }

  public SimpleConfig getSimpleConfig() {
    return simpleConfig;
  }

  public void setSimpleConfig(SimpleConfig simpleConfig) {
    this.simpleConfig = simpleConfig;
  }

  public void setAdvancedConfig(AdvancedConfig advancedConfig) {
    this.advancedConfig = advancedConfig;
  }
}
