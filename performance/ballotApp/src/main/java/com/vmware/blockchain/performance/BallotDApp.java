package com.vmware.blockchain.performance;

import com.vmware.blockchain.samples.Ballot;

import okhttp3.OkHttpClient;
import okhttp3.logging.HttpLoggingInterceptor;
import okhttp3.logging.HttpLoggingInterceptor.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.web3j.crypto.Credentials;
import org.web3j.crypto.ECKeyPair;
import org.web3j.crypto.Keys;
import org.web3j.crypto.RawTransaction;
import org.web3j.crypto.TransactionEncoder;
import org.web3j.protocol.Web3j;
import org.web3j.protocol.core.DefaultBlockParameterName;
import org.web3j.protocol.core.methods.response.EthBlock.Block;
import org.web3j.protocol.core.methods.response.EthGetTransactionReceipt;
import org.web3j.protocol.http.HttpService;
import org.web3j.tx.gas.DefaultGasProvider;
import org.web3j.utils.Numeric;

import java.io.File;
import java.io.FileWriter;
import java.io.PrintWriter;

import java.math.BigInteger;

import java.text.DecimalFormat;

import java.util.*;
import java.util.Map.Entry;
import java.util.concurrent.*;

public class BallotDApp {
	private static final Logger logger = LogManager.getLogger(BallotDApp.class);
	public static String ENDPOINT = "";
	public static String PASSWORD = "Test123456";
	private static String PROPOSAL_DATA_PATH = "data/proposals";
	public static String RESULT_PATH = "result";
	public static String DEPLOYER_KEY_PATH = "data/deployer_keystore";
	private static String PERFORMANCE_DATA = RESULT_PATH + "/performance_result.log";
	private static String PERFORMANCE_DATA_CSV = RESULT_PATH + "/performance.csv";
	public static String WAVEFRONT_DATA_PATH = RESULT_PATH + "/wavefrontData.txt";
	private static  String CONTRACT_DATA_PATH = RESULT_PATH + "/contract";
	public static String STAT_DATA_PATH = RESULT_PATH + "/stats.log";
	public static int NUMBER = 1000;
	public static boolean ENABLE_LOGGING = false;
	private static double response;
	private Web3j web3j;
	public static OkHttpClient CLIENT = null;
	public static int RATE_CONTROL = 0;
	public static int DRIVERID = -1;
	public static String RUNID = "";
	public static int NUMBER_THREADS = Runtime.getRuntime().availableProcessors()*2;
	private List<Entry<String, Integer>> weightedEndpoints = null;
	public static boolean CONCORD = true;
	private Map<String,List<String> > stats;
	private String testName;
	DecimalFormat df = new DecimalFormat("#.00");
	public static int PORT = 8545;
	public static String CONCORD_USERNAME = "admin@blockchain.local";
	public static String CONCORD_PASSWORD = "Admin!23";
	public static BigInteger GAS_PRICE = DefaultGasProvider.GAS_PRICE;
	public static String blockchainType = "concord";
	public static boolean http;

	public void setWavefrontDataPath(String waveFrontDataPath) {
		BallotDApp.WAVEFRONT_DATA_PATH = waveFrontDataPath;
	}

	public void setPerformanceData(String performanceData) {
		BallotDApp.PERFORMANCE_DATA = performanceData;
	}

	public void setEndpoints(List<Entry<String, Integer>> endpoints) {
		this.weightedEndpoints = endpoints;
	}

	public void setPerformanceDataCSV(String performanceDataCSV) {
		BallotDApp.PERFORMANCE_DATA_CSV = performanceDataCSV;
	}

	public String getPerformanceData() {
		return BallotDApp.PERFORMANCE_DATA;
	}

	public void setContractDataPath(String contractDataPath) {
		BallotDApp.CONTRACT_DATA_PATH = contractDataPath;
	}

	public String getContractDataPath() {
		return BallotDApp.CONTRACT_DATA_PATH;
	}

	public Map<String,List<String> > getStats() {
		return this.stats;
	}

	public void setTestName(String name){
		this.testName = name;
	}

	//Logging interceptor to see JSON_RPC callls
	private static final HttpLoggingInterceptor loggingInterceptor =
			new HttpLoggingInterceptor((msg) -> {
				logger.debug(msg);
			});
	static {
		loggingInterceptor.setLevel(Level.BODY);
	}

	public static HttpLoggingInterceptor getLoggingInterceptor() {
		return loggingInterceptor;
	}


	public void process(Credentials[] credentials) {
		try{
			processingVoting(credentials);
		}
		catch(Exception e) {
			e.printStackTrace();
		}

	}

	public static void main(String[] args) {
		if (args.length > 0) {

			if (args[0].equals("--helen")) {
				String helenIP = args[1];
				ENDPOINT = ((http)?"http://":"https://") + helenIP + ":8080/api/concord/eth";
				logger.info("Connected to Helen at " + ENDPOINT);
			} else if (args[0].equals("--concord")) {
				String concordIP = args[1];
				ENDPOINT = ((http)?"http://":"https://") + concordIP + ":" + PORT;
				logger.info("Connected to Concord at " + ENDPOINT);
			} else {
				logger.error("Please specify the connection endpoint. (--helen or --concord as 1st arg)");
				return;
			}

			NUMBER = Integer.valueOf(args[2]);

			String dataPath = args[3];
			PROPOSAL_DATA_PATH = dataPath;

			RESULT_PATH = args[4];
                        logger.info("RESULT_PATH = '"+RESULT_PATH+"'");
			DEPLOYER_KEY_PATH = RESULT_PATH + "/deployer_keystore";
			File keystorePath = new File(DEPLOYER_KEY_PATH);
			if (!keystorePath.exists()) {
				keystorePath.mkdirs();
			}
			CONTRACT_DATA_PATH = RESULT_PATH + "/contract";
			PERFORMANCE_DATA = RESULT_PATH + "/performance_result.log";
			PERFORMANCE_DATA_CSV = RESULT_PATH + "/performance.csv";
                        STAT_DATA_PATH = RESULT_PATH + "/stats.log";
                        WAVEFRONT_DATA_PATH = RESULT_PATH + "/wavefrontData.txt";

			for (int i = 0; i < args.length; i++) {
				if (args[i].equals("--logging")) {
					ENABLE_LOGGING = true;
					logger.info("Logging enabled");
					break;
				}
			}

			try {
				CLIENT = Utils.getClient();
			} catch (Exception e) {
				e.printStackTrace();
			}

		}

		try {
			BallotDApp dapp = new BallotDApp();

			BallotDeployer deployer = new BallotDeployer();
			deployer.deploy(PROPOSAL_DATA_PATH, CONTRACT_DATA_PATH, BallotDApp.PASSWORD, BallotDApp.DEPLOYER_KEY_PATH);

			Credentials[] credentials = dapp.generateCredentialsForVoter();
			response = deployer.grantRightToVote(credentials);

			dapp.processingVoting(credentials);

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private void processingVoting(Credentials[] credentials) throws Exception {
		logger.info("Number of threads: " + NUMBER_THREADS);

		List<Voting> votings = new ArrayList<>();
		logger.info("Number of Transactions: " + NUMBER);
		long startVoting = System.nanoTime();

		/**
		 * The nonce for every transaction is set to 0. This is the correct choice in the current setup
		 * because every benchmark is run with freshly generated voters. If this changes then the
		 * implementation should be updated to include an increasing nonce counter.
		 * The current implementation saves a call to the network for every voter we create. This
		 * greatly improves voter generation time.
		 */
		BigInteger nonce = BigInteger.ZERO;
		Map<String, Web3j> serviceMap = new HashMap<>();
		ScheduledExecutorService scheduledExecutorService = new ScheduledThreadPoolExecutor(NUMBER_THREADS);
		if (CONCORD) {
                    if (weightedEndpoints == null && BallotDApp.ENDPOINT != null) {
                        String concordIP = BallotDApp.ENDPOINT.split("/")[2].split(":")[0];
                        weightedEndpoints = new ArrayList<Entry<String,Integer>>(1);
                        Map.Entry<String,Integer> entry =
                            new AbstractMap.SimpleEntry<String, Integer>(concordIP, NUMBER);
                        weightedEndpoints.add(entry);
                    }
			if (weightedEndpoints != null) {
				ArrayList<Integer> numTxs = new ArrayList<Integer>(weightedEndpoints.size());

				//extract values from weightedEndpoints list
				for (int i = 0; i < weightedEndpoints.size(); i++) {
					numTxs.add(weightedEndpoints.get(i).getValue());
				}

				int credentialIndex = 0;
				int nodeIndex = 0;
				while (credentialIndex < credentials.length) {
					if (nodeIndex == numTxs.size()) {
						nodeIndex = 0;
					} else {
						if (numTxs.get(nodeIndex) != 0) {
							//create the transaction with IP from weightedEndpoints
							String currentIp = ((http)?"http://":"https://") + weightedEndpoints.get(nodeIndex).getKey() + ":" + PORT;
							if (!serviceMap.containsKey(currentIp))  {
								HttpService httpServiceEth = new HttpService(currentIp, BallotDApp.CLIENT, false);
								httpServiceEth.addHeader("Authorization", okhttp3.Credentials.basic(CONCORD_USERNAME, CONCORD_PASSWORD));
								Web3j web3jObj = Web3j.build(httpServiceEth, 15000L, scheduledExecutorService);
								serviceMap.put(currentIp, web3jObj);
							}
							web3j = serviceMap.get(currentIp);

							Ballot ballot = Utils.loadContract(web3j, CONTRACT_DATA_PATH, credentials[credentialIndex]);
							Voting voting = new Voting(web3j, ballot, credentials[credentialIndex], currentIp);
							String data = voting.encode(voting.getProposal());
							RawTransaction rawTransaction = RawTransaction.createTransaction(nonce,
									DefaultGasProvider.GAS_PRICE,
									DefaultGasProvider.GAS_LIMIT,
									ballot.getContractAddress(),
									BigInteger.valueOf(0),
									data);
							byte[] signedMsg = TransactionEncoder.signMessage(rawTransaction, credentials[credentialIndex]);
							String hexValue = Numeric.toHexString(signedMsg);
							voting.setSignedMsg(hexValue);
							votings.add(voting);
							credentialIndex++;
							numTxs.set(nodeIndex, numTxs.get(nodeIndex)-1);
						}
						nodeIndex++;
					}
				}
			}
		} else {
			//If benchmark specifies Helen
			for (Credentials credential : credentials){
				HttpService httpServiceEth = new HttpService(BallotDApp.ENDPOINT, BallotDApp.CLIENT, false);
				httpServiceEth.addHeader("Authorization", okhttp3.Credentials.basic(CONCORD_USERNAME, CONCORD_PASSWORD));
				web3j = Web3j.build(httpServiceEth);

				Ballot ballot = Utils.loadContract(web3j, CONTRACT_DATA_PATH, credential);
				Voting voting = new Voting(web3j, ballot, credential, BallotDApp.ENDPOINT);
				String data = voting.encode(voting.getProposal());
				String signedMsg = voting.sign(data, nonce);
				voting.setSignedMsg(signedMsg);
				votings.add(voting);
			}
		}
		scheduledExecutorService.shutdown();
		scheduledExecutorService.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS);
		long endVoting = System.nanoTime();
		logger.info("Time to create Voters is: " + (endVoting - startVoting));
		Block block = web3j.ethGetBlockByNumber(DefaultBlockParameterName.LATEST, false).send().getBlock();
		logger.info("Block Number before actual voting " + block.getNumber().toString());
		PrintWriter writer = new PrintWriter(new FileWriter(new File(PERFORMANCE_DATA)));
		PrintWriter writerCSV = new PrintWriter(new FileWriter(new File(PERFORMANCE_DATA_CSV)));

		long sleepTime = 0;
		if (RATE_CONTROL != 0) {
			sleepTime = 1_000_000_000/RATE_CONTROL;
		}
		


		logger.info("Starting Transactions..");

		ExecutorService excutor = Executors.newFixedThreadPool(NUMBER_THREADS);
		List<Future<long[]> > resultList = new ArrayList<>();
		for(int i = 0; i < votings.size(); i++)
		    resultList.add(i, null);
		int index = 0;

		long startTime = System.nanoTime();
        writer.println("Transactions were started at: " + startTime);
		for (int i = 0; i < votings.size(); i++) {
			Future<long[]> result = excutor.submit(votings.get(i));
			resultList.set(i, result);
			if(RATE_CONTROL != 0)
				Utils.applyRateControl(sleepTime, index, startTime);
			index++;
		}

		excutor.shutdown();
		excutor.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS);
		long endTime = System.nanoTime();
		logger.info("Finished executing all Transactions");
        writer.println("Transactions were finished at: " + endTime);
		long total_time = endTime - startTime;


		ArrayList<Long> latencies = new ArrayList<>();
		for(int i = 0; i < resultList.size(); i++) {
			latencies.add(resultList.get(i).get()[1] - resultList.get(i).get()[0]);
		}

		double avg = 0.0;

		for(Long x: latencies) {
			avg += x/NUMBER;
		}
		avg = avg/1000000.0;

		logger.debug("Number of transactions: " + NUMBER);
		logger.info("Rate control value: " + RATE_CONTROL);
		logger.info("Total time taken: " + total_time/1000000000.0 + " s");
		logger.info("Avergae Latency : " + avg + " ms");
		logger.info("Throughput: " + (NUMBER*1.0/total_time)*1000000000 + " tps");


		/*
		 * All completed transactions are stored in memory and only written to file when ALL of them finished
		 * They are ordered by earliest completion time first
		 */
		writerCSV.println("Txid, Duration (nsec)");
		writerCSV.println();
		logger.debug(WAVEFRONT_DATA_PATH);

		File wavefrontFile = new File(WAVEFRONT_DATA_PATH);
		if (wavefrontFile.createNewFile())
		{
			logger.info("File to dump data for wavefront is created!");
		} else {
			logger.info("File to dump data for wavefront already exists.");
		}
		FileWriter waveFrontFileWriter = new FileWriter(wavefrontFile);

		for (int i = 0; i < latencies.size(); i++) {
			writerCSV.println((i + 1) + "," +latencies.get(i) + "," + votings.get(i).nodeIp);
			waveFrontFileWriter.write("ballot.app.tx" + (i+1) + ".latency " + String.format("%.02f", latencies.get(i)/1000000.0) + " " + java.time.Instant.now().getEpochSecond() + " " + " source=" + DRIVERID+ " driverId="+DRIVERID+ " runId="+RUNID + "\n");
		}


		int numSuccessfulTransactions = latencies.size();
		Collections.sort(latencies);
		waveFrontFileWriter.write("ballot.app.transaction.p50 " + String.format("%.02f", latencies.get(numSuccessfulTransactions/2)/1000000.0) + " " + java.time.Instant.now().getEpochSecond() + " " + " source=" + DRIVERID+ " driverId="+DRIVERID+ " runId="+RUNID + "\n");
		waveFrontFileWriter.write("ballot.app.transaction.p95 " + String.format("%.02f", latencies.get(numSuccessfulTransactions*95/100)/1000000.0) + " " + java.time.Instant.now().getEpochSecond() + " " + " source=" + DRIVERID+ " driverId="+DRIVERID+ " runId="+RUNID + "\n");
		waveFrontFileWriter.write("ballot.app.transaction.p99 " + String.format("%.02f", latencies.get(numSuccessfulTransactions*99/100)/1000000.0) + " " + java.time.Instant.now().getEpochSecond() + " " + " source=" + DRIVERID+ " driverId="+DRIVERID+ " runId="+RUNID + "\n");
		waveFrontFileWriter.close();

//		Populate stats
		Collections.sort(latencies);
		stats = new HashMap<>();
		stats.put("tableRow", Arrays.asList(testName, String.valueOf(numSuccessfulTransactions), df.format(100*numSuccessfulTransactions*1.0/NUMBER) + "%" ,String.valueOf(NUMBER - numSuccessfulTransactions), RATE_CONTROL + " tps", df.format(latencies.get(latencies.size() - 1)/1000000.0) + " ms",
				df.format(latencies.get(0)/1000000.0) + " ms", df.format(avg) + " ms", df.format((latencies.size()*1.0/total_time)*1000000000) + " tx/sec"));



		writer.printf("Response Time: %.4f\n", response);
		writer.printf("Transaction Rate: %.2f tx/sec\n", (latencies.size()*1.0/total_time)*1000000000);

		votings.clear();
		writer.close();
		writerCSV.close();
	}

	public void processingVotingEthereum(Credentials[] credentials) throws Exception {

		logger.info("Ethereum Transactions");
		logger.info("Number of threads: " + NUMBER_THREADS);
		logger.info("Number of Transactions: " + NUMBER);

		long startVoting = System.nanoTime();
		List<Voting> votings = new ArrayList<>();
		if (weightedEndpoints != null) {
			ArrayList<Integer> numTxs = new ArrayList<Integer>(weightedEndpoints.size());

			//extract values from weightedEndpoints list
			for (int i = 0; i < weightedEndpoints.size(); i++) {
				numTxs.add(weightedEndpoints.get(i).getValue());
			}

			int credentialIndex = 0;
			int nodeIndex = 0;
			while (credentialIndex < credentials.length) {
				if (nodeIndex == numTxs.size()) {
					nodeIndex = 0;
				} else {
					if (numTxs.get(nodeIndex) != 0) {
						//create the transaction with IP from weightedEndpoints
						String currentIp = ((http)?"http://":"https://") + weightedEndpoints.get(nodeIndex).getKey() + ":" + PORT;
						HttpService httpServiceEthereum = new HttpService(currentIp, BallotDApp.CLIENT, false);
						httpServiceEthereum.addHeader("Authorization", okhttp3.Credentials.basic(CONCORD_USERNAME, CONCORD_PASSWORD));
						web3j = Web3j.build(httpServiceEthereum);

						Ballot ballot = Utils.loadContract(web3j, CONTRACT_DATA_PATH, credentials[credentialIndex]);
						Voting voting = new Voting(web3j, ballot, credentials[credentialIndex], currentIp);
						String data = voting.encode(voting.getProposal());
						String signedMsg = voting.sign(data, BigInteger.ZERO);
						voting.setSignedMsg(signedMsg);
						votings.add(voting);
						credentialIndex++;
						numTxs.set(nodeIndex, numTxs.get(nodeIndex)-1);
					}
					nodeIndex++;
				}

			}

		}
		long endVoting = System.nanoTime();
		logger.info("Time to create Voters is: " + (endVoting - startVoting));

		PrintWriter writer = new PrintWriter(new FileWriter(PERFORMANCE_DATA));
		PrintWriter writerCSV = new PrintWriter(new FileWriter(new File(PERFORMANCE_DATA_CSV)));

		ExecutorService executor = Executors.newFixedThreadPool(NUMBER_THREADS);

		List<AsyncTransaction> time = Collections.synchronizedList(new ArrayList<AsyncTransaction>(votings.size()));
		List<CompletableFuture<AsyncTransaction>> tasks = new ArrayList<>();

		Block block = web3j.ethGetBlockByNumber(DefaultBlockParameterName.LATEST, false).send().getBlock();
		logger.info("Block Number before Transactions " + block.getNumber().toString());
		
		long startMillisTime = System.currentTimeMillis();
		
		long conStart = System.nanoTime();
		long sleepTime = 0;
		if (RATE_CONTROL != 0) {
			sleepTime = 1_000_000_000/RATE_CONTROL;
		}
		long timeStartLoop = System.nanoTime();
		long start = System.nanoTime();
		for (int i = 0; i < votings.size(); i++) {
			CompletableFuture<AsyncTransaction> task =
					votings.get(i).executeEthereum(executor, i).whenComplete((entry, error) -> {
						if (error != null) {
							logger.error("Error occurred", error);
						} else {
							entry.setDiverId(DRIVERID);
							time.add(entry);
						}
					});

			tasks.add(task);

			if (RATE_CONTROL != 0) {
				Utils.applyRateControl(sleepTime, i, start);
			}
		}

		executor.shutdown();

		/**
		 * The list of all transactions is synchronized. Transactions receipts will be checked right away.
		 * To wait for all asynchronous transactions add the two following lines:
		 * CompletableFuture<Void> allDone = CompletableFuture.allOf(tasks.toArray(new CompletableFuture[0]));
		 * CompletableFuture.allOf(allDone).join();
		 */

		long conEnd = System.nanoTime();
		logger.info("Mining Transactions..");

		//For Quorum we are only checking if the last 2% of transactions have been mined. (This can be changed based on requirements)
		if (blockchainType.equals("quorum")) {
			CompletableFuture<Void> allDone = CompletableFuture.allOf(tasks.toArray(new CompletableFuture[0]));
			CompletableFuture.allOf(allDone).join();

			int txCommited = 0;
			//change 0.02 corresponds to 2% of transactions
			int numOfTxToCheck = (int) Math.max(1, NUMBER*0.02);
			int index = NUMBER-numOfTxToCheck;

			while (txCommited < numOfTxToCheck) {
				if (index == time.size()) {
					index = NUMBER-numOfTxToCheck;
					TimeUnit.MILLISECONDS.sleep(10);
				}
				AsyncTransaction currentTransaction = time.get(index);
				if (currentTransaction.getCompleted() == false) {
					EthGetTransactionReceipt transactionReceipt = web3j.ethGetTransactionReceipt(currentTransaction.finishedTx.getTransactionHash()).send();
					//check if transaction is in a block
					if(transactionReceipt.getTransactionReceipt().isPresent()) {
						currentTransaction.setCompleted(true);
						currentTransaction.setEndTime(System.nanoTime());
						txCommited++;
						time.set(index, currentTransaction);
					}
				}
				index++;
			}
		} else {
			//check if ALL transactions are mined
			int count = 0;
			int index = 0;
			while (count < NUMBER) {

				if (time.size() == 0) {
					index = 0;
					TimeUnit.MILLISECONDS.sleep(100); 
				}
				int testTxIndex = index%(NUMBER/5);
				if (testTxIndex == 0 && testTxIndex < time.size()) {
					count = count + NUMBER/5;
				}

				AsyncTransaction currentTransaction = time.get(index);
				if (currentTransaction.getCompleted() == false) {
					EthGetTransactionReceipt transactionReceipt = web3j.ethGetTransactionReceipt(currentTransaction.finishedTx.getTransactionHash()).send();
					if(transactionReceipt.getTransactionReceipt().isPresent()) {

						count++;
						currentTransaction.setCompleted(true);
						currentTransaction.setEndTime(System.nanoTime());
						if (count%(NUMBER/10) == 0) {
							logger.info("Finished mining " + count + " Transactions" + " (" + (NUMBER-count + " to go)"));
						}
						time.set(index, currentTransaction);
					}
					
				}
				index++;
			}
		}

		//After this point all transactions have been mined
		long timeEndLoop = System.nanoTime();
		long endMillisTime = System.currentTimeMillis();
		
		Block blockEnd = web3j.ethGetBlockByNumber(DefaultBlockParameterName.LATEST, false).send().getBlock();
		
		writer.println("Transactions were started at: " + startMillisTime + " (millis)");
		logger.info("Transactions were started at: " + startMillisTime + " (millis)");
		logger.info("Block Number after Transactions " + blockEnd.getNumber().toString());
		
		logger.info("Finished starting all Transactions");
		logger.info("Concurrency Start Time is: " + conStart);
		logger.info("Concurrency End time is: " + conEnd);
		logger.info("Concurrency Total Time is: " + (conEnd - conStart));
		writer.printf("Concurrency Total Time is: %d\n", conEnd - conStart);
		writer.println("Transactions were finished at: " + endMillisTime + " (millis)");
		logger.info("Transactions were finished at: " + endMillisTime + " (millis)");

		long totalSum = 0;
		long minStartTime = Long.MAX_VALUE;
		long maxEndTime = Long.MIN_VALUE;

		Collections.sort(time, new Comparator<AsyncTransaction>() {
			@Override
			public int compare(AsyncTransaction tx1, AsyncTransaction tx2) {
				return Long.compare(tx1.end, tx2.end);
			}

		});

		/*
		 * All completed transactions are stored in memory and only written to file when ALL of them finished
		 * They are ordered by earliest completion time first
		 */
		writerCSV.println("DriverID,NodeIP,ID,Start (relative to first tx in nsec),End (sorted by first completion),Duration (nsec)");
		writerCSV.println();
		logger.debug(WAVEFRONT_DATA_PATH);
		File wavefrontFile = new File(WAVEFRONT_DATA_PATH);
		if (wavefrontFile.createNewFile())
		{
			logger.info("File to dump data for wavefront is created!");
		} else {
			logger.info("File to dump data for wavefront already exists.");
		}
		FileWriter waveFrontFileWriter = new FileWriter(wavefrontFile);

		ArrayList<Long> latencies = new ArrayList<Long>(votings.size());

		for (int i = 0; i < time.size(); i++) {

			if (time.get(i).start < minStartTime) {
				minStartTime = time.get(i).start;
			}
			if (time.get(i).end > maxEndTime) {
				maxEndTime = time.get(i).end;
			}

			long duration = time.get(i).end - time.get(i).start;
			latencies.add(duration);

			writerCSV.println(time.get(i).getDriverId() + "," + time.get(i).getNodeIp() + "," + time.get(i).getId() + "," + (time.get(i).start-minStartTime) + "," + (time.get(i).end-minStartTime) + "," + duration);
			waveFrontFileWriter.write("ballot.app.tx" + time.get(i).getId() + ".latency " + String.format("%.02f", duration/1000000.0) + " source=" + time.get(i).getDriverId()+ " driverId="+DRIVERID+ " runId="+RUNID+ " nodeIP="+ parseIP(time.get(i).getNodeIp()) + "\n");
			totalSum += duration;
		}
		Collections.sort(latencies);
		waveFrontFileWriter.write("ballot.app.transaction.p50 " + String.format("%.02f", latencies.get(NUMBER/2)/1000000.0) + " source=" + DRIVERID+"\n");
		waveFrontFileWriter.write("ballot.app.transaction.p95 " + String.format("%.02f", latencies.get(NUMBER*95/100)/1000000.0) + " source=" + DRIVERID +"\n");
		waveFrontFileWriter.write("ballot.app.transaction.p99 " + String.format("%.02f", latencies.get(NUMBER*99/100)/1000000.0) + " source=" + DRIVERID+"\n");
		waveFrontFileWriter.close();


		logger.info("Average time response time: " + totalSum*1.0/NUMBER);
		logger.info("Start time of processing Voting: " + minStartTime);
		logger.info("End time of processing Voting: " + maxEndTime);
		logger.info("Total time for process: " + (maxEndTime - minStartTime) + " nano seconds");
		logger.info("Start/End Transaction Rate: " + NUMBER/((maxEndTime - minStartTime)/1000000000.0) + " tx/sec");
		logger.info("Transaction Rate: " + NUMBER/((timeEndLoop - timeStartLoop)/1000000000.0) + " tx/sec");

		writer.printf("Response Time: %.4f\n", response);
		writer.printf("Burst Response Time: %.4f\n", totalSum*1.0e-9/NUMBER);
		writer.printf("Transaction Rate: %.2f tx/sec\n", NUMBER/((maxEndTime - minStartTime)*1.0e-9));

		votings.clear();
		tasks.clear();
		time.clear();

		writer.close();
		writerCSV.close();
	}

	public String parseIP(String url) {
		int colonBeforePortIndex = url.lastIndexOf(":");
		int slashAfterHttpIndex = url.lastIndexOf("/");
		String ip = "";
		for(int i = slashAfterHttpIndex; i < colonBeforePortIndex; i++)
			ip += url.charAt(i);
		return ip;
	}

	public Credentials[] getCredentials() {
		try {
			return generateCredentialsForVoter();
		}
		catch(Exception e) {
			e.printStackTrace();
		}
		return null;
	}

	private Credentials[] generateCredentialsForVoter() throws Exception {
		Credentials[] credentials = new Credentials[NUMBER];
		for (int i = 0; i < NUMBER; i++) {
			ECKeyPair ecKeyPair = Keys.createEcKeyPair();
			credentials[i] = Credentials.create(ecKeyPair);

		}
		return credentials;
	}

}
