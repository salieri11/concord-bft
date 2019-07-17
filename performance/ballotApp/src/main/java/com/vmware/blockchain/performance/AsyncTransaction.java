package com.vmware.blockchain.performance;

import java.util.concurrent.CompletableFuture;

import org.web3j.protocol.Web3j;
import org.web3j.protocol.core.methods.response.EthSendTransaction;

public class AsyncTransaction implements Runnable {

	private String signedMsg;
	public long start;
	public long end;
	private Web3j web3j;
	private int id;
	private String nodeIp;
	private int driverId;
	
	private boolean completed = false;
	public CompletableFuture<EthSendTransaction> ethSendTransaction;
	public EthSendTransaction finishedTx;

	public AsyncTransaction(Web3j web3j, String signedMsg) {
		this.signedMsg = signedMsg;
		this.web3j = web3j;
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

	public void setId(int Id) {
		this.id = Id;
	}

	public int getId() {
		return id;
	}

	public void setNodeIp(String nodeIp) {
		this.nodeIp = nodeIp;
	}

	public String getNodeIp() {
		return nodeIp;
	}

	public void setDiverId(int driverId) {
		this.driverId = driverId;
	}

	public int getDriverId() {
		return driverId;
	}
	
	public void setCompleted(boolean completed) {
		this.completed = completed;
	}
	
	public boolean getCompleted() {
		return completed;
	}
}
