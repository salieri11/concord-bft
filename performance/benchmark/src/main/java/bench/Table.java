package bench;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class Table {
  private List<String> tableHeader;
  private List<Map<String, List<String>>> tableData;

  public Table() {
    this.tableData = new ArrayList<>();
  }

  public void setTableHeader(List<String> header) {
    this.tableHeader = header;
  }

  public List<String> getTableHeader() {
    return this.tableHeader;
  }

  public void addTableData(Map<String, List<String>> resultsData) {
    this.tableData.add(resultsData);
  }

  public List<Map<String, List<String>>> getTableData() {
    return this.tableData;
  }
}
