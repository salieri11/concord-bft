package dappbench;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Data {
    private Summary summary;
    private String configFilePath;
    private List<Map<String, String> > basicInformation;


    public Data() {
        this.summary = new Summary();
        this.basicInformation = new ArrayList<>();
    }

    public void setSummaryTableHeader(List<String> headers) {
        this.summary.setTableHeader(headers);
    }

    public void addSummaryTableData(Map<String, List<String> > resultsData) {
        this.summary.addTableData(resultsData);
    }

    public Summary getSummary() {
        return this.summary;
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
}
