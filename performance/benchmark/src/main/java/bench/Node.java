package bench;

import static java.util.Objects.requireNonNull;

public class Node {
  String ip;
  int port;
  int percentage;

  public String getIp() {
    return requireNonNull(ip, "Node IP can not be null");
  }

  public void setIp(String ip) {
    this.ip = ip;
  }

  public int getPort() {
    return port;
  }

  public void setPort(int port) {
    this.port = port;
  }

  public int getPercentage() {
    return percentage;
  }

  public void setPercentage(int percentage) {
    this.percentage = percentage;
  }

  @Override
  public String toString() {
      return "Node {" + "ip=" + ip + ", port=" + port + "}";
  }
}
