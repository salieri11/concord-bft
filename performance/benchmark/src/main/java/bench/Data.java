package bench;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Data {
  private Table appPerforamnceTable;
  private String configFilePath;
  private List<Map<String, String>> basicInformation;

  public Data() {
    this.appPerforamnceTable = new Table();
    this.basicInformation = new ArrayList<>();
  }

  public void setAppSummaryTableHeader(List<String> headers) {
    this.appPerforamnceTable.setTableHeader(headers);
  }

  public void addAppSummaryTableData(Map<String, List<String>> resultsData) {
    this.appPerforamnceTable.addTableData(resultsData);
  }

  public Table getAppSummary() {
    return this.appPerforamnceTable;
  }

  public void setConfigFilePath(String path) {
    this.configFilePath = path;
  }

  String getConfigFilePath() {
    return this.configFilePath;
  }

  public void addBasicInformation(String name, String value) {
    Map<String, String> basicInfo = new HashMap<>();
    basicInfo.put("name", name);
    basicInfo.put("value", value);
    basicInformation.add(basicInfo);
  }

  List<Map<String, String>> getBasicInformation() {
    return this.basicInformation;
  }
}
