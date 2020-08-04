package bench;

import java.util.List;

public class SimpleConfig {
  private String blockchain;
  private int nodeCount;
  private List<Node> nodes;
  private List<Workload> workloads;
  private int sleepTime;
  private int port;
  private boolean http = false;
  private int numberThreads = 0;

  public String getBlockchain() {
    return blockchain;
  }

  public void setBlockchain(String blockchain) {
    this.blockchain = blockchain;
  }

  public int getNodeCount() {
    return nodeCount;
  }

  public void setNodeCount(int nodeCount) {
    this.nodeCount = nodeCount;
  }

  public List<Node> getNodes() {
    return nodes;
  }

  public void setNodes(List<Node> nodes) {
    this.nodes = nodes;
  }

  public int getNumberThreads() {
    return numberThreads;
  }

  public void setNumberThreads(int threads) {
    this.numberThreads = threads;
  }

  public String getOutputDir() {
    return System.getProperty("RESULT_DIR", "perf_result");
  }

  public List<Workload> getWorkloads() {
    return workloads;
  }

  public void setWorkloads(List<Workload> workloads) {
    this.workloads = workloads;
  }

  public int getSleepTime() {
    return sleepTime;
  }

  public void setSleepTime(int sleepTime) {
    this.sleepTime = sleepTime;
  }

  public int getPort() {
    return port;
  }

  public void setPort(int port) {
    this.port = port;
  }

  public void setHttp(boolean http) {
    this.http = http;
  }

  public boolean isHttp() {
    return http;
  }
}
