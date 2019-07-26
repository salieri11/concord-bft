package dappbench;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Data {
    private Table appPerforamnceTable;
    private List<Map<String, Object> > dockerTables;
    private String configFilePath;
    private List<Map<String, String> > basicInformation;


    public Data() {
        this.appPerforamnceTable = new Table();
        this.dockerTables = new ArrayList<>();
        this.basicInformation = new ArrayList<>();
    }

    public void setAppSummaryTableHeader(List<String> headers) {
        this.appPerforamnceTable.setTableHeader(headers);
    }

    public void addAppSummaryTableData(Map<String, List<String> > resultsData) {
        this.appPerforamnceTable.addTableData(resultsData);
    }

    public Table getAppSummary() {
        return this.appPerforamnceTable;
    }

    public void setConfigFilePath(String path){
        this.configFilePath = path;
    }

    public String getConfigFilePath(){
        return this.configFilePath;
    }

    public void addBasicInformation(String name, String value){
        Map<String, String> basicInfo = new HashMap<>();
        basicInfo.put("name",name);
        basicInfo.put("value", value);
        basicInformation.add(basicInfo);
    }

    public List<Map<String, String> > getBasicInformation() {
        return this.basicInformation;
    }

    public void addDockerInfo(Map<String, Object> info) {
        this.dockerTables.add(info);
    }



    public List<Map<String, Object> > getDockerInfo() {
        return this.dockerTables;
    }
}
