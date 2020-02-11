package bench;

import java.util.List;

public class Workload {
  private String dapp;
  private String language;
  private int numOfRuns;
  private boolean logging = false;
  private int rateControl = 0;
  private List<String> params;

  public String getDapp() {
    return dapp;
  }

  public void setDapp(String dapp) {
    this.dapp = dapp;
  }

  public String getLanguage() {
    return language;
  }

  public void setLanguage(String language) {
    this.language = language;
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
