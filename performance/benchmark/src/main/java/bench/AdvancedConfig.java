package bench;

public class AdvancedConfig {
  private Wavefront wavefront = null;
  private String concordUsername = "admin@blockchain.local";
  private String concordPassword = "Admin!23";

  public Wavefront getWavefront() {
    return wavefront;
  }

  public void setWavefront(Wavefront wavefront) {
    this.wavefront = wavefront;
  }

  public String getConcordUsername() {
    return concordUsername;
  }

  public void setConcordUsername(String username) {
    this.concordUsername = username;
  }

  public String getConcordPassword() {
    return concordPassword;
  }

  public void setConcordPassword(String password) {
    this.concordPassword = password;
  }

  public static class Wavefront {

    private boolean enabled;
    private String proxyHost;
    private int metricsPort;
    private String source;
    private String metricName;

    public boolean isEnabled() {
      return enabled;
    }

    public void setEnabled(boolean enabled) {
      this.enabled = enabled;
    }

    public String getProxyHost() {
      return proxyHost;
    }

    public void setProxyHost(String proxyHost) {
      this.proxyHost = proxyHost;
    }

    public int getMetricsPort() {
      return metricsPort;
    }

    public void setMetricsPort(int metricsPort) {
      this.metricsPort = metricsPort;
    }

    public String getSource() {
      return source;
    }

    public void setSource(String source) {
      this.source = source;
    }

    public String getMetricName() {
      return metricName;
    }

    public void setMetricName(String metricName) {
      this.metricName = metricName;
    }
  }
}
