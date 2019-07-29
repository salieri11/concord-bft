package com.vmware.blockchain.performance;

import com.vmware.blockchain.samples.Ballot;

import org.web3j.abi.FunctionEncoder;
import org.web3j.abi.TypeReference;
import org.web3j.abi.datatypes.Function;
import org.web3j.abi.datatypes.Type;
import org.web3j.crypto.Credentials;
import org.web3j.crypto.RawTransaction;
import org.web3j.crypto.TransactionEncoder;
import org.web3j.protocol.Web3j;
import org.web3j.protocol.http.HttpService;
import org.web3j.tx.gas.DefaultGasProvider;
import org.web3j.utils.Numeric;

import java.math.BigInteger;

import java.util.Arrays;
import java.util.Collections;
import java.util.Random;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;

public class Voting {

	private Ballot ballot;
	private Web3j web3j;

	private Credentials credentials;

	private String signedMsg;

	private BigInteger proposal;

	public String nodeIp;

	public Voting(Web3j web3j, Ballot ballot, Credentials credentials, String nodeIp) {
		this.web3j = web3j;
		this.ballot = ballot;
		this.credentials = credentials;
		this.nodeIp = nodeIp;
		Random random = new Random(System.currentTimeMillis());
		int index = random.nextInt(2);
		proposal = BigInteger.valueOf(index);
	}

	public Voting(String path, Credentials credentials) throws Exception {
		this.web3j = Web3j.build(new HttpService(BallotDApp.ENDPOINT));
		this.credentials = credentials;
		this.ballot = Utils.loadContract(web3j, path, credentials);
	}

	public CompletableFuture<AsyncTransaction> execute(AsyncTransaction asyncTransaction, ExecutorService executor) {
		final CompletableFuture<AsyncTransaction> promise = new CompletableFuture<>();
		CompletableFuture.runAsync(asyncTransaction, executor).thenRun(() -> {
			asyncTransaction.setEndTime(System.nanoTime());
			promise.complete(asyncTransaction);
		});
		return promise;
	}
	
	public CompletableFuture<AsyncTransaction> executeEthereum(ExecutorService executor, int id) throws Exception {
		AsyncTransaction tx = new AsyncTransaction(web3j, signedMsg);
		tx.setId(id);
		tx.setNodeIp(nodeIp);
		final CompletableFuture<AsyncTransaction> promise = new CompletableFuture<>();
		CompletableFuture.runAsync(tx, executor).thenRun(() -> {
			try {
				tx.finishedTx = tx.ethSendTransaction.get();
				tx.ethSendTransaction = null;
			} catch (Exception e) {
				e.printStackTrace();
			}
			promise.complete(tx);
		});
		return promise;
	}

	@SuppressWarnings("rawtypes")
	public String encode(BigInteger proposal) {
		final Function function = new Function(
				Ballot.FUNC_VOTE,
				Arrays.<Type>asList(new org.web3j.abi.datatypes.generated.Uint256(proposal)),
				Collections.<TypeReference<?>>emptyList());
		return FunctionEncoder.encode(function);
	}

	public String sign(String data, BigInteger nonce) throws Exception {
		RawTransaction rawTransaction = RawTransaction.createTransaction(nonce,
				DefaultGasProvider.GAS_PRICE,
				DefaultGasProvider.GAS_LIMIT,
				ballot.getContractAddress(),
				BigInteger.valueOf(0),
				data);

		byte[] signedMsg = TransactionEncoder.signMessage(rawTransaction, credentials);
		String hexValue = Numeric.toHexString(signedMsg);
		return hexValue;
	}

	public String getSignedMsg() {
		return signedMsg;
	}

	public void setSignedMsg(String message) {
		this.signedMsg = message;
	}

	public BigInteger getProposal() {
		return proposal;
	}

	public String getNodeIp() {
		return nodeIp;
	}

}
