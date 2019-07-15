package com.vmware.blockchain.performance;

import com.vmware.blockchain.samples.Ballot;

import okhttp3.OkHttpClient;

import org.apache.logging.log4j.*;

import org.web3j.crypto.Credentials;
import org.web3j.protocol.Web3j;
import org.web3j.protocol.core.methods.response.TransactionReceipt;
import org.web3j.protocol.http.HttpService;
import org.web3j.tx.gas.DefaultGasProvider;

import java.io.*;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class BallotDeployer {

	private static final Logger logger = LogManager.getLogger(BallotDApp.class);

	private Credentials credentials;
	private Ballot ballot;
	public BallotDeployer() {

	}

	public void deploy(String proposalPath, String contractPath) throws Exception {

		OkHttpClient client = BallotDApp.CLIENT;

		HttpService httpServiceEth = new HttpService(BallotDApp.ENDPOINT, client, false);
		System.out.println(BallotDApp.ENDPOINT);
		httpServiceEth.addHeader("Authorization", okhttp3.Credentials.basic(BallotDApp.CONCORD_USERNAME, BallotDApp.CONCORD_PASSWORD));
		Web3j web3j = Web3j.build(httpServiceEth);

		logger.info("Connected to Ethereum client version: "
				+ web3j.web3ClientVersion().send().getWeb3ClientVersion());

		// load credential
		credentials = Utils.loadCredential(BallotDApp.PASSWORD, BallotDApp.DEPLOYER_KEY_PATH);

		List<byte[]> proposals = Utils.getProposals(proposalPath);

		try {
			long deployStartTime = System.nanoTime();;
			ballot = Ballot.deploy(web3j, credentials, DefaultGasProvider.GAS_PRICE, DefaultGasProvider.GAS_LIMIT, proposals).send();
			long deployEndTime = System.nanoTime();
			PrintWriter writer = new PrintWriter(new BufferedWriter(new FileWriter(BallotDApp.STAT_DATA_PATH, true)));
			writer.println("STAT_DEPLOY_CONTRACT_LATENCY=" + (deployEndTime-deployStartTime) + " ns");
			writer.close();
			logger.log(Level.getLevel("STAT"), "STAT_DEPLOY_CONTRACT_LATENCY=" + (deployEndTime-deployStartTime) + " ns");

		} catch(IOException e) {
			e.printStackTrace();
		} catch(Exception e) {
			logger.info(e.getMessage());
		}


		String contractAddress = ballot.getContractAddress();
		logger.info("Smart contract deployed to address " + contractAddress);

		BufferedWriter writer = new BufferedWriter(new FileWriter(new File(contractPath)));
		writer.write(contractAddress);
		writer.close();

		TransactionReceipt receipt = ballot.getTransactionReceipt().get();
		logger.info("Transaction info about deploying contract:\n" + receipt);
	}

	public double grantRightToVote(Credentials[] credentials) throws Exception {
		long startTime = System.nanoTime();
		List<CompletableFuture<TransactionReceipt>> tasks = new ArrayList<>();
		ExecutorService executor = Executors.newFixedThreadPool(BallotDApp.NUMBER_THREADS);
		for (Credentials credential : credentials)  {
			CompletableFuture<TransactionReceipt> task =
					execute(executor, ballot, credential.getAddress()).whenComplete((entry, error) -> {
						if (error != null) {
							logger.error("Error occurred during granting right: ", error);
						}
					});

			tasks.add(task);
		}
		executor.shutdown();

		CompletableFuture<Void> allDone =
				CompletableFuture.allOf(tasks.toArray(new CompletableFuture[0]));
		CompletableFuture.allOf(allDone).join();
		long endTime = System.nanoTime();

		logger.info("Total Time for Granting Right is " + (endTime - startTime) + " nano seconds");
		if (BallotDApp.NUMBER != 0) {
			return (endTime - startTime) *(1.0e-9) / BallotDApp.NUMBER;
		}
		return 0;
		
	}
	
	public CompletableFuture<TransactionReceipt> execute(ExecutorService executor, Ballot ballot, String voter) {
		final CompletableFuture<TransactionReceipt> promise = new CompletableFuture<>();
		CompletableFuture.runAsync(() -> {
			try {
				ballot.giveRightToVote(voter).sendAsync();
			} catch (Exception e) {
				promise.completeExceptionally(e);
			}
			
		}, executor).thenRun(() -> {
			promise.complete(new TransactionReceipt());
		});
		return promise;
	}
}
