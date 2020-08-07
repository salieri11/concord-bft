package bench;

import java.util.List;

public class Workload {
  private String dapp;
  private String language;
  private int numOfRuns;
  private boolean logging = false;
  private int rateControl = 0;
  private int logUpdateFreq = 0;
  private List<String> params;

  public String getDapp() {
    return dapp;
  }

  public void setDapp(String dapp) {
    this.dapp = dapp;
  }

  public int getNumOfRuns() {
    return numOfRuns;
  }

  public void setNumOfRuns(int numOfRuns) {
    this.numOfRuns = numOfRuns;
  }

  public boolean getLogging() {
    return logging;
  }

  public void setLogging(boolean logging) {
    this.logging = logging;
  }

  public int getLogUpdateFreq() {
    return logUpdateFreq;
  }

  public void setLogUpdateFreq(int logUpdateFreq) {
    this.logUpdateFreq = logUpdateFreq;
  }

  public int getRateControl() {
    return rateControl;
  }

  public void setRateControl(int rateControl) {
    this.rateControl = rateControl;
  }

  public List<String> getParams() {
    return params;
  }

  public void setParams(List<String> params) {
    this.params = params;
  }
}
