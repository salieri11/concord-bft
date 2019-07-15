package com.vmware.blockchain.performance;

import org.web3j.protocol.Web3j;

public class AsyncTransaction implements Runnable {

	private String signedMsg;
	public long start;
	public long end;
	private Web3j web3j;
	private int id;
	private String nodeIp;
	private int driverId;

	public AsyncTransaction(Web3j web3j, String signedMsg) {
		this.signedMsg = signedMsg;
		this.web3j = web3j;
	}

	@Override
	public void run() {
		start = System.nanoTime();
		web3j.ethSendRawTransaction(signedMsg).sendAsync();	
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
}
