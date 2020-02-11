package ballotapp;

import org.web3j.protocol.Web3j;
import org.web3j.protocol.core.methods.response.EthSendTransaction;

import java.util.concurrent.CompletableFuture;

public class AsyncTransaction implements Runnable {

  public long start;
  public long end;
  public CompletableFuture<EthSendTransaction> ethSendTransaction;
  public EthSendTransaction finishedTx;
  private String signedMsg;
  private Web3j web3j;
  private int id;
  private String nodeIp;
  private int driverId;
  private boolean completed = false;

  public AsyncTransaction(Web3j web3j, String signedMsg) {
    this.signedMsg = signedMsg;
    this.web3j = web3j;
  }

  public AsyncTransaction(Web3j web3j, String signedMsg, int id, String nodeIp) {
    this(web3j, signedMsg);
    this.id = id;
    this.nodeIp = nodeIp;
  }

  @Override
  public void run() {
    start = System.nanoTime();
    ethSendTransaction = web3j.ethSendRawTransaction(signedMsg).sendAsync();
  }

  public long getStartTime() {
    return start;
  }

  public void setStart(long start) {
    this.start = start;
  }

  public long getEndTime() {
    return end;
  }

  public void setEndTime(long end) {
    this.end = end;
  }

  public int getId() {
    return id;
  }

  public void setId(int Id) {
    this.id = Id;
  }

  public String getNodeIp() {
    return nodeIp;
  }

  public void setNodeIp(String nodeIp) {
    this.nodeIp = nodeIp;
  }

  public void setDiverId(int driverId) {
    this.driverId = driverId;
  }

  public int getDriverId() {
    return driverId;
  }

  public boolean getCompleted() {
    return completed;
  }

  public void setCompleted(boolean completed) {
    this.completed = completed;
  }
}
