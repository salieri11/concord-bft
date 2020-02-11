package ballotapp;

import bench.BenchUtil;
import okhttp3.OkHttpClient;
import okhttp3.logging.HttpLoggingInterceptor;
import okhttp3.logging.HttpLoggingInterceptor.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.web3j.crypto.*;
import org.web3j.protocol.Web3j;
import org.web3j.protocol.core.DefaultBlockParameterName;
import org.web3j.protocol.core.methods.response.EthBlock.Block;
import org.web3j.protocol.core.methods.response.EthGetTransactionReceipt;
import org.web3j.protocol.http.HttpService;
import org.web3j.tx.gas.DefaultGasProvider;
import org.web3j.utils.Numeric;
import samples.Ballot;

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
  static String ENDPOINT = "";
  static String PASSWORD = "Test123456";
  private static String RESULT_PATH = "result";
  static String DEPLOYER_KEY_PATH = "data/deployer_keystore";
  private static String PERFORMANCE_DATA = RESULT_PATH + "/performance_result.log";
  private static String PERFORMANCE_DATA_CSV = RESULT_PATH + "/performance.csv";
  static String WAVEFRONT_DATA_PATH = RESULT_PATH + "/wavefrontData.txt";
  private static String CONTRACT_DATA_PATH = RESULT_PATH + "/contract";
  static String STAT_DATA_PATH = RESULT_PATH + "/stats.log";
  static int NUMBER = 1000;
  static boolean ENABLE_LOGGING = false;
  private Web3j web3j;
  static OkHttpClient CLIENT = null;
  static int RATE_CONTROL = 0;
  static int DRIVERID = -1;
  static String RUNID = "";
  static int NUMBER_THREADS = Runtime.getRuntime().availableProcessors() * 2;
  private List<Entry<String, Integer>> weightedEndpoints = null;
  private Map<String, List<String>> stats;
  private String testName;
  private DecimalFormat df = new DecimalFormat("#.00");
  static int PORT = 8545;
  static String CONCORD_USERNAME = "admin@blockchain.local";
  static String CONCORD_PASSWORD = "Admin!23";
  static BigInteger GAS_PRICE = DefaultGasProvider.GAS_PRICE;
  static String blockchainType = "concord";
  public static boolean http;

  void setPerformanceData(String performanceData) {
    BallotDApp.PERFORMANCE_DATA = performanceData;
  }

  void setEndpoints(List<Entry<String, Integer>> endpoints) {
    this.weightedEndpoints = endpoints;
  }

  void setPerformanceDataCSV(String performanceDataCSV) {
    BallotDApp.PERFORMANCE_DATA_CSV = performanceDataCSV;
  }

  void setContractDataPath(String contractDataPath) {
    BallotDApp.CONTRACT_DATA_PATH = contractDataPath;
  }

  String getContractDataPath() {
    return BallotDApp.CONTRACT_DATA_PATH;
  }

  Map<String, List<String>> getStats() {
    return this.stats;
  }

  void setTestName(String name) {
    this.testName = name;
  }

  // Logging interceptor to see JSON_RPC callls
  private static final HttpLoggingInterceptor loggingInterceptor =
      new HttpLoggingInterceptor(
          (msg) -> {
            logger.debug(msg);
          });

  static {
    loggingInterceptor.setLevel(Level.BODY);
  }

  static HttpLoggingInterceptor getLoggingInterceptor() {
    return loggingInterceptor;
  }

  /**
   * Utility function to create Voters, it uses threads to create voters simultaneously
   *
   * @param credentials
   * @param votings
   * @throws Exception
   */
  private void createVoters(Credentials[] credentials, List<Voting> votings) throws Exception {
    /*
     * The nonce for every transaction is set to 0. This is the correct choice in the current setup
     * because every benchmark is run with freshly generated voters. If this changes then the
     * implementation should be updated to include an increasing nonce counter. The current
     * implementation saves a call to the network for every voter we create. This greatly improves
     * voter generation time.
     */
    BigInteger nonce = BigInteger.ZERO;
    Map<String, Web3j> serviceMap = new HashMap<>();
    ScheduledExecutorService scheduledExecutorService =
        new ScheduledThreadPoolExecutor(NUMBER_THREADS);

    if (weightedEndpoints == null && ENDPOINT != null) {
      String concordIP = ENDPOINT.split("/")[2].split(":")[0];
      weightedEndpoints = new ArrayList<Entry<String, Integer>>(1);
      Map.Entry<String, Integer> entry =
          new AbstractMap.SimpleEntry<String, Integer>(concordIP, NUMBER);
      weightedEndpoints.add(entry);
    }
    if (weightedEndpoints != null) {
      ArrayList<Integer> numTxs = new ArrayList<Integer>(weightedEndpoints.size());

      // extract values from weightedEndpoints list
      for (Entry<String, Integer> weightedEndpoint : weightedEndpoints) {
        numTxs.add(weightedEndpoint.getValue());
      }

      int credentialIndex = 0;
      int nodeIndex = 0;
      while (credentialIndex < credentials.length) {
        if (nodeIndex == numTxs.size()) {
          nodeIndex = 0;
        } else {
          if (numTxs.get(nodeIndex) != 0) {
            // create the transaction with IP from weightedEndpoints
            String currentIp =
                ((http) ? "http://" : "https://")
                    + weightedEndpoints.get(nodeIndex).getKey()
                    + ":"
                    + PORT;
            if (!serviceMap.containsKey(currentIp)) {
              HttpService httpServiceEth = new HttpService(currentIp, CLIENT, false);
              httpServiceEth.addHeader(
                  "Authorization", okhttp3.Credentials.basic(CONCORD_USERNAME, CONCORD_PASSWORD));
              Web3j web3jObj = Web3j.build(httpServiceEth, 15000L, scheduledExecutorService);
              serviceMap.put(currentIp, web3jObj);
            }
            web3j = serviceMap.get(currentIp);

            samples.Ballot ballot =
                Utils.loadContract(web3j, CONTRACT_DATA_PATH, credentials[credentialIndex]);
            Voting voting = new Voting(web3j, ballot, credentials[credentialIndex], currentIp);
            String data = voting.encode(voting.getProposal());
            RawTransaction rawTransaction =
                RawTransaction.createTransaction(
                    nonce,
                    DefaultGasProvider.GAS_PRICE,
                    DefaultGasProvider.GAS_LIMIT,
                    ballot.getContractAddress(),
                    BigInteger.valueOf(0),
                    data);
            byte[] signedMsg =
                TransactionEncoder.signMessage(rawTransaction, credentials[credentialIndex]);
            String hexValue = Numeric.toHexString(signedMsg);
            voting.setSignedMsg(hexValue);
            votings.add(voting);
            credentialIndex++;
            numTxs.set(nodeIndex, numTxs.get(nodeIndex) - 1);
          }
          nodeIndex++;
        }
      }
    }

    scheduledExecutorService.shutdown();
    scheduledExecutorService.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS);
  }

  /**
   * function to process voting request for concord
   *
   * @param credentials
   * @throws Exception
   */
  void processingVoting(Credentials[] credentials) throws Exception {
    logger.info("Number of threads: " + NUMBER_THREADS);

    List<Voting> votings = new ArrayList<>();
    logger.info("Number of Transactions: " + NUMBER);
    long startVoting = System.nanoTime();

    createVoters(credentials, votings);

    long endVoting = System.nanoTime();
    logger.info("Time to create Voters is: " + (endVoting - startVoting));
    Block block =
        web3j.ethGetBlockByNumber(DefaultBlockParameterName.LATEST, false).send().getBlock();
    logger.info("Block Number before actual voting " + block.getNumber().toString());
    PrintWriter writer = new PrintWriter(new FileWriter(new File(PERFORMANCE_DATA)));
    PrintWriter writerCSV = new PrintWriter(new FileWriter(new File(PERFORMANCE_DATA_CSV)));

    long sleepTime = 0;
    if (RATE_CONTROL != 0) {
      sleepTime = 1_000_000_000 / RATE_CONTROL;
    }

    logger.info("Starting Transactions..");

    ExecutorService excutor = Executors.newFixedThreadPool(NUMBER_THREADS);
    List<Future<long[]>> resultList = new ArrayList<>();
    for (int i = 0; i < votings.size(); i++) resultList.add(i, null);
    int index = 0;

    long startTime = System.nanoTime();
    writer.println("Transactions were started at: " + startTime);
    for (int i = 0; i < votings.size(); i++) {
      Future<long[]> result = excutor.submit(votings.get(i));
      resultList.set(i, result);
      if (RATE_CONTROL != 0) BenchUtil.applyRateControl(sleepTime, index, startTime);
      index++;
    }

    excutor.shutdown();
    excutor.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS);
    long endTime = System.nanoTime();
    logger.info("Finished executing all Transactions");
    writer.println("Transactions were finished at: " + endTime);
    long total_time = endTime - startTime;

    ArrayList<Long> latencies = new ArrayList<>();
    for (Future<long[]> future : resultList) {
      latencies.add(future.get()[1] - future.get()[0]);
    }

    double avg = 0.0;

    for (Long x : latencies) {
      avg += (double) x / NUMBER;
    }
    avg = avg / 1000000.0;

    logger.debug("Number of transactions: " + NUMBER);
    logger.info("Rate control value: " + RATE_CONTROL);
    logger.info("Total time taken: " + total_time / 1000000000.0 + " s");
    logger.info("Avergae Latency : " + avg + " ms");
    logger.info("Throughput: " + (NUMBER * 1.0 / total_time) * 1000000000 + " tps");

    /*
     * All completed transactions are stored in memory and only written to file when ALL of them finished
     * They are ordered by earliest completion time first
     */
    writerCSV.println("Txid, Duration (nsec)");
    writerCSV.println();

    for (int i = 0; i < latencies.size(); i++) {
      writerCSV.println((i + 1) + "," + latencies.get(i) + "," + votings.get(i).nodeIp);
    }

    Collections.sort(latencies);
    writeToWaveFront(latencies);
    int numSuccessfulTransactions = latencies.size();

    //		Populate stats
    Collections.sort(latencies);
    stats = new HashMap<>();
    stats.put(
        "tableRow",
        Arrays.asList(
            testName,
            String.valueOf(numSuccessfulTransactions),
            df.format(100 * numSuccessfulTransactions * 1.0 / NUMBER) + "%",
            String.valueOf(NUMBER - numSuccessfulTransactions),
            RATE_CONTROL + " tps",
            df.format(latencies.get(latencies.size() - 1) / 1000000.0) + " ms",
            df.format(latencies.get(0) / 1000000.0) + " ms",
            df.format(avg) + " ms",
            df.format((latencies.size() * 1.0 / total_time) * 1000000000) + " tx/sec"));

    writer.printf(
        "Transaction Rate: %.2f tx/sec\n", (latencies.size() * 1.0 / total_time) * 1000000000);

    votings.clear();
    writer.close();
    writerCSV.close();
  }

  /**
   * Helper function to create Wavefront Writer object
   *
   * @return
   * @throws Exception
   */
  private FileWriter createWaveFrontWriter() throws Exception {
    logger.debug(WAVEFRONT_DATA_PATH);

    File wavefrontFile = new File(WAVEFRONT_DATA_PATH);

    if (wavefrontFile.createNewFile()) {
      logger.info("File to dump data for wavefront is created!");
    } else {
      logger.info("File to dump data for wavefront already exists.");
    }

    return new FileWriter(wavefrontFile);
  }

  /**
   * function to create wavefront dump for concord
   *
   * @param latencies
   * @throws Exception
   */
  private void writeToWaveFront(ArrayList<Long> latencies) throws Exception {

    FileWriter waveFrontFileWriter = createWaveFrontWriter();

    int numSuccessfulTransactions = latencies.size();

    for (int i = 0; i < latencies.size(); i++) {
      waveFrontFileWriter.write(
          "ballot.app.tx"
              + (i + 1)
              + ".latency "
              + String.format("%.02f", latencies.get(i) / 1000000.0)
              + " "
              + java.time.Instant.now().getEpochSecond()
              + " "
              + " source="
              + DRIVERID
              + " driverId="
              + DRIVERID
              + " runId="
              + RUNID
              + "\n");
    }

    waveFrontFileWriter.write(
        "ballot.app.transaction.p50 "
            + String.format("%.02f", latencies.get(numSuccessfulTransactions / 2) / 1000000.0)
            + " "
            + java.time.Instant.now().getEpochSecond()
            + " "
            + " source="
            + DRIVERID
            + " driverId="
            + DRIVERID
            + " runId="
            + RUNID
            + "\n");
    waveFrontFileWriter.write(
        "ballot.app.transaction.p95 "
            + String.format(
                "%.02f", latencies.get(numSuccessfulTransactions * 95 / 100) / 1000000.0)
            + " "
            + java.time.Instant.now().getEpochSecond()
            + " "
            + " source="
            + DRIVERID
            + " driverId="
            + DRIVERID
            + " runId="
            + RUNID
            + "\n");
    waveFrontFileWriter.write(
        "ballot.app.transaction.p99 "
            + String.format(
                "%.02f", latencies.get(numSuccessfulTransactions * 99 / 100) / 1000000.0)
            + " "
            + java.time.Instant.now().getEpochSecond()
            + " "
            + " source="
            + DRIVERID
            + " driverId="
            + DRIVERID
            + " runId="
            + RUNID
            + "\n");
    waveFrontFileWriter.close();
  }

  /**
   * create voters for ethereum based blockchain.
   *
   * @param credentials
   * @param votings
   * @throws Exception
   */
  private void createVotersEthereum(Credentials[] credentials, List<Voting> votings)
      throws Exception {

    if (weightedEndpoints != null) {
      ArrayList<Integer> numTxs = new ArrayList<Integer>(weightedEndpoints.size());

      // extract values from weightedEndpoints list
      for (Entry<String, Integer> weightedEndpoint : weightedEndpoints) {
        numTxs.add(weightedEndpoint.getValue());
      }

      int credentialIndex = 0;
      int nodeIndex = 0;
      while (credentialIndex < credentials.length) {
        if (nodeIndex == numTxs.size()) {
          nodeIndex = 0;
        } else {
          if (numTxs.get(nodeIndex) != 0) {
            // create the transaction with IP from weightedEndpoints
            String currentIp =
                ((http) ? "http://" : "https://")
                    + weightedEndpoints.get(nodeIndex).getKey()
                    + ":"
                    + PORT;
            HttpService httpServiceEthereum = new HttpService(currentIp, CLIENT, false);
            httpServiceEthereum.addHeader(
                "Authorization", okhttp3.Credentials.basic(CONCORD_USERNAME, CONCORD_PASSWORD));
            web3j = Web3j.build(httpServiceEthereum);

            Ballot ballot =
                Utils.loadContract(web3j, CONTRACT_DATA_PATH, credentials[credentialIndex]);
            Voting voting = new Voting(web3j, ballot, credentials[credentialIndex], currentIp);
            String data = voting.encode(voting.getProposal());
            String signedMsg = voting.sign(data, BigInteger.ZERO);
            voting.setSignedMsg(signedMsg);
            votings.add(voting);
            credentialIndex++;
            numTxs.set(nodeIndex, numTxs.get(nodeIndex) - 1);
          }
          nodeIndex++;
        }
      }
    }
  }

  /**
   * function to process voting request for Ethereum
   *
   * @param credentials
   * @throws Exception
   */
  void processingVotingEthereum(Credentials[] credentials) throws Exception {

    logger.info("Ethereum Transactions");
    logger.info("Number of threads: " + NUMBER_THREADS);
    logger.info("Number of Transactions: " + NUMBER);

    long startVoting = System.nanoTime();
    List<Voting> votings = new ArrayList<>();
    createVotersEthereum(credentials, votings);

    long endVoting = System.nanoTime();
    logger.info("Time to create Voters is: " + (endVoting - startVoting));

    PrintWriter writer = new PrintWriter(new FileWriter(PERFORMANCE_DATA));
    PrintWriter writerCSV = new PrintWriter(new FileWriter(new File(PERFORMANCE_DATA_CSV)));

    ExecutorService executor = Executors.newFixedThreadPool(NUMBER_THREADS);

    List<AsyncTransaction> time = Collections.synchronizedList(new ArrayList<>(votings.size()));
    List<CompletableFuture<AsyncTransaction>> tasks = new ArrayList<>();

    Block block =
        web3j.ethGetBlockByNumber(DefaultBlockParameterName.LATEST, false).send().getBlock();
    logger.info("Block Number before Transactions " + block.getNumber().toString());

    long startMillisTime = System.currentTimeMillis();

    long conStart = System.nanoTime();
    long sleepTime = 0;
    if (RATE_CONTROL != 0) {
      sleepTime = 1_000_000_000 / RATE_CONTROL;
    }
    long timeStartLoop = System.nanoTime();
    long start = System.nanoTime();
    for (int i = 0; i < votings.size(); i++) {
      CompletableFuture<AsyncTransaction> task =
          votings
              .get(i)
              .executeEthereum(executor, i)
              .whenComplete(
                  (entry, error) -> {
                    if (error != null) {
                      logger.error("Error occurred", error);
                    } else {
                      entry.setDiverId(DRIVERID);
                      time.add(entry);
                    }
                  });

      tasks.add(task);

      if (RATE_CONTROL != 0) {
        BenchUtil.applyRateControl(sleepTime, i, start);
      }
    }

    executor.shutdown();

    /*
     * The list of all transactions is synchronized. Transactions receipts will be checked right
     * away. To wait for all asynchronous transactions add the two following lines:
     * CompletableFuture<Void> allDone = CompletableFuture.allOf(tasks.toArray(new
     * CompletableFuture[0])); CompletableFuture.allOf(allDone).join();
     */
    long conEnd = System.nanoTime();
    logger.info("Mining Transactions..");

    mineTransaction(tasks, time);

    // After this point all transactions have been mined
    long timeEndLoop = System.nanoTime();
    long endMillisTime = System.currentTimeMillis();

    Block blockEnd =
        web3j.ethGetBlockByNumber(DefaultBlockParameterName.LATEST, false).send().getBlock();

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

    time.sort(
        new Comparator<AsyncTransaction>() {
          @Override
          public int compare(AsyncTransaction tx1, AsyncTransaction tx2) {
            return Long.compare(tx1.end, tx2.end);
          }
        });

    /*
     * All completed transactions are stored in memory and only written to file when ALL of them finished
     * They are ordered by earliest completion time first
     */
    writerCSV.println(
        "DriverID,NodeIP,ID,Start (relative to first tx in nsec),End (sorted by first completion),Duration (nsec)");
    writerCSV.println();

    ArrayList<Long> latencies = new ArrayList<>(votings.size());

    for (AsyncTransaction asyncTransaction : time) {

      if (asyncTransaction.start < minStartTime) {
        minStartTime = asyncTransaction.start;
      }
      if (asyncTransaction.end > maxEndTime) {
        maxEndTime = asyncTransaction.end;
      }

      long duration = asyncTransaction.end - asyncTransaction.start;
      latencies.add(duration);

      writerCSV.println(
          asyncTransaction.getDriverId()
              + ","
              + asyncTransaction.getNodeIp()
              + ","
              + asyncTransaction.getId()
              + ","
              + (asyncTransaction.start - minStartTime)
              + ","
              + (asyncTransaction.end - minStartTime)
              + ","
              + duration);

      totalSum += duration;
    }
    writeToWaveFrontEthreum(latencies, time);

    logger.info("Average time response time: " + totalSum * 1.0 / NUMBER);
    logger.info("Start time of processing Voting: " + minStartTime);
    logger.info("End time of processing Voting: " + maxEndTime);
    logger.info("Total time for process: " + (maxEndTime - minStartTime) + " nano seconds");
    logger.info(
        "Start/End Transaction Rate: "
            + NUMBER / ((maxEndTime - minStartTime) / 1000000000.0)
            + " tx/sec");
    logger.info(
        "Transaction Rate: " + NUMBER / ((timeEndLoop - timeStartLoop) / 1000000000.0) + " tx/sec");

    writer.printf("Burst Response Time: %.4f\n", totalSum * 1.0e-9 / NUMBER);
    writer.printf(
        "Transaction Rate: %.2f tx/sec\n", NUMBER / ((maxEndTime - minStartTime) * 1.0e-9));

    votings.clear();
    tasks.clear();
    time.clear();

    writer.close();
    writerCSV.close();
  }

  /**
   * Function to mine all the transaction
   *
   * @param tasks
   * @param time
   * @throws Exception
   */
  private void mineTransaction(
      List<CompletableFuture<AsyncTransaction>> tasks, List<AsyncTransaction> time)
      throws Exception {
    // For Quorum we are only checking if the last 2% of transactions have been mined. (This can be
    // changed based on requirements)
    if (blockchainType.equals("quorum")) {
      CompletableFuture<Void> allDone =
          CompletableFuture.allOf(tasks.toArray(new CompletableFuture[0]));
      CompletableFuture.allOf(allDone).join();

      int txCommited = 0;
      // change 0.02 corresponds to 2% of transactions
      int numOfTxToCheck = (int) Math.max(1, NUMBER * 0.02);
      int index = NUMBER - numOfTxToCheck;

      while (txCommited < numOfTxToCheck) {
        if (index == time.size()) {
          index = NUMBER - numOfTxToCheck;
          TimeUnit.MILLISECONDS.sleep(10);
        }
        AsyncTransaction currentTransaction = time.get(index);
        if (!currentTransaction.getCompleted()) {
          EthGetTransactionReceipt transactionReceipt =
              web3j
                  .ethGetTransactionReceipt(currentTransaction.finishedTx.getTransactionHash())
                  .send();
          // check if transaction is in a block
          if (transactionReceipt.getTransactionReceipt().isPresent()) {
            currentTransaction.setCompleted(true);
            currentTransaction.setEndTime(System.nanoTime());
            txCommited++;
            time.set(index, currentTransaction);
          }
        }
        index++;
      }
    } else {
      // check if ALL transactions are mined
      int count = 0;
      int index = 0;
      while (count < NUMBER) {

        if (time.size() == 0) {
          index = 0;
          TimeUnit.MILLISECONDS.sleep(100);
        }
        int testTxIndex = index % (NUMBER / 5);
        if (testTxIndex == 0 && testTxIndex < time.size()) {
          count = count + NUMBER / 5;
        }

        AsyncTransaction currentTransaction = time.get(index);
        if (!currentTransaction.getCompleted()) {
          EthGetTransactionReceipt transactionReceipt =
              web3j
                  .ethGetTransactionReceipt(currentTransaction.finishedTx.getTransactionHash())
                  .send();
          if (transactionReceipt.getTransactionReceipt().isPresent()) {

            count++;
            currentTransaction.setCompleted(true);
            currentTransaction.setEndTime(System.nanoTime());
            if (count % (NUMBER / 10) == 0) {
              logger.info(
                  "Finished mining "
                      + count
                      + " Transactions"
                      + " ("
                      + (NUMBER - count + " to go)"));
            }
            time.set(index, currentTransaction);
          }
        }
        index++;
      }
    }
  }

  private void writeToWaveFrontEthreum(ArrayList<Long> latencies, List<AsyncTransaction> time)
      throws Exception {

    FileWriter waveFrontFileWriter = createWaveFrontWriter();

    for (AsyncTransaction asyncTransaction : time) {
      long duration = asyncTransaction.end - asyncTransaction.start;
      waveFrontFileWriter.write(
          "ballot.app.tx"
              + asyncTransaction.getId()
              + ".latency "
              + String.format("%.02f", duration / 1000000.0)
              + " source="
              + asyncTransaction.getDriverId()
              + " driverId="
              + DRIVERID
              + " runId="
              + RUNID
              + " nodeIP="
              + parseIP(asyncTransaction.getNodeIp())
              + "\n");
    }
    Collections.sort(latencies);
    waveFrontFileWriter.write(
        "ballot.app.transaction.p50 "
            + String.format("%.02f", latencies.get(NUMBER / 2) / 1000000.0)
            + " source="
            + DRIVERID
            + "\n");
    waveFrontFileWriter.write(
        "ballot.app.transaction.p95 "
            + String.format("%.02f", latencies.get(NUMBER * 95 / 100) / 1000000.0)
            + " source="
            + DRIVERID
            + "\n");
    waveFrontFileWriter.write(
        "ballot.app.transaction.p99 "
            + String.format("%.02f", latencies.get(NUMBER * 99 / 100) / 1000000.0)
            + " source="
            + DRIVERID
            + "\n");
    waveFrontFileWriter.close();
  }

  /**
   * parse IP from the url
   *
   * @param url
   * @return
   */
  private String parseIP(String url) {
    int colonBeforePortIndex = url.lastIndexOf(":");
    int slashAfterHttpIndex = url.lastIndexOf("/");
    String ip = "";
    for (int i = slashAfterHttpIndex; i < colonBeforePortIndex; i++) {
      ip += url.charAt(i);
    }
    return ip;
  }

  Credentials[] getCredentials() {
    try {
      return generateCredentialsForVoter();
    } catch (Exception e) {
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
