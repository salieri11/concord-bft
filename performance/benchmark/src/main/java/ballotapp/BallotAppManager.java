package ballotapp;

import bench.*;
import org.apache.logging.log4j.Logger;
import org.web3j.crypto.Credentials;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.*;

import static org.apache.logging.log4j.LogManager.getLogger;

public class BallotAppManager {

  private static final Logger logger = getLogger(BallotAppManager.class);
  private List<Node> nodes;
  private Workload workload;
  private int numberOfTransactions;
  private List<Map.Entry<String, Integer>> weightedEndpoints;
  private SimpleConfig simpleConfig;
  private AdvancedConfig advancedConfig;
  private BallotDApp dapp;
  private String dataPath;
  private String results;
  private Data data;

  public BallotAppManager(
      SimpleConfig simpleConfig, AdvancedConfig advancedConfig, List<Node> nodes) {
    List<Workload> workloads = simpleConfig.getWorkloads();
    workload = workloads.get(0);
    // generate a unique driverID for this run
    BallotDApp.DRIVERID = new Random().nextInt(999999);
    BallotDApp.PORT = simpleConfig.getPort();
    // advanced configuration
    BallotDApp.CONCORD_USERNAME = advancedConfig.getConcordUsername();
    BallotDApp.CONCORD_PASSWORD = advancedConfig.getConcordUsername();
    BallotDApp.http = simpleConfig.isHttp();
    if (simpleConfig.getNumberThreads() != 0) {
      BallotDApp.NUMBER_THREADS = simpleConfig.getNumberThreads();
    } else {
      BallotDApp.NUMBER_THREADS = (Runtime.getRuntime().availableProcessors() * 2);
    }
    if (workload.getLogging()) BallotDApp.ENABLE_LOGGING = true;

    BallotDApp.RATE_CONTROL = workload.getRateControl();

    this.nodes = nodes;
    this.simpleConfig = simpleConfig;
    this.advancedConfig = advancedConfig;
    results = simpleConfig.getOutputDir();

    weightedEndpoints = new ArrayList<Map.Entry<String, Integer>>();
    dapp = new BallotDApp();
    List<String> workloadParams = workload.getParams();

    BallotDApp.ENDPOINT =
        ((simpleConfig.isHttp()) ? "http://" : "https://")
            + workloadParams.get(1)
            + ":"
            + simpleConfig.getPort();
    numberOfTransactions = Integer.parseInt(workloadParams.get(2));
    dataPath = workloadParams.get(3);

    // Will be used in reporting
    data = new Data();
    data.setAppSummaryTableHeader(
        Arrays.asList(
            "Workload-Itr",
            "Succ",
            "Succ Rate",
            "Fail",
            "Send Rate",
            "Max Latency",
            "Min Latency",
            "Avg Latency",
            "Throughput"));
    data.addBasicInformation("DLT", simpleConfig.getBlockchain());
    data.setConfigFilePath("../" + BenchUtil.getConfigPath());
    logger.debug("Finished setting up constructor");
  }

  public void processTransaction() throws Exception {
    int workloadNum = 1;

    for (int node = 0; node < nodes.size(); node++) {
      if (simpleConfig.getNodes().get(node).getPercentage() != 0) {
        String initBlockNumber =
            BallotAppUtil.getBlockNumber(simpleConfig.getNodes().get(node).getIp(), simpleConfig);
        logger.info(
            "Block number on "
                + simpleConfig.getNodes().get(node).getIp()
                + " before processing any transaction: "
                + initBlockNumber);
      }
    }

    int totalWorkloads = 0;
    try {
      for (int i = 0; i < workload.getNumOfRuns(); i++) {

        dapp.setTestName(workloadNum + "-" + (i + 1));
        BallotDApp.RUNID = new SimpleDateFormat("yyyyMMddHHmmss").format(new Date());

        dapp.setEndpoints(weightedEndpoints);

        String PROPOSAL_DATA_PATH = dataPath;
        String resultPath = results + "/workload" + totalWorkloads;
        BallotDApp.DEPLOYER_KEY_PATH = resultPath + "/deployer_keystore";
        File keystorePath = new File(BallotDApp.DEPLOYER_KEY_PATH);
        if (!keystorePath.exists()) {
          keystorePath.mkdirs();
        }
        dapp.setContractDataPath(resultPath + "/contract");
        String filename = new SimpleDateFormat("yyyyMMddHHmm").format(new Date());
        dapp.setPerformanceData(resultPath + "/performance_result-" + filename + ".log");
        dapp.setPerformanceDataCSV(resultPath + "/performance-" + filename + ".csv");

        BallotDApp.WAVEFRONT_DATA_PATH = resultPath + "/wavefront/" + filename + ".log";
        BallotDApp.STAT_DATA_PATH = resultPath + "/stat-" + filename + ".log";
        File wavefrontPath = new File(resultPath + "/wavefront/");
        if (!wavefrontPath.exists()) {
          wavefrontPath.mkdirs();
        }

        BallotDApp.CLIENT = Utils.getClient();
        BallotDeployer deployer = new BallotDeployer();
        deployer.deploy(
            PROPOSAL_DATA_PATH,
            dapp.getContractDataPath(),
            BallotDApp.PASSWORD,
            BallotDApp.DEPLOYER_KEY_PATH);

        Credentials[] credentials = dapp.getCredentials();

        if (simpleConfig.getBlockchain().equals("ethereum")) {
          deployer.grantRightToVoteEthereum(credentials);
          dapp.processingVotingEthereum(credentials);
        } else {
          deployer.grantRightToVote(credentials);
          dapp.processingVoting(credentials);
        }

        System.gc();
        Thread.sleep(simpleConfig.getSleepTime());
        totalWorkloads++;
        data.addAppSummaryTableData(dapp.getStats());
      }
    } catch (Exception e) {
      e.printStackTrace();
      throw e;
    }
    Reporting report = new Reporting(data);
    report.process(simpleConfig.getOutputDir());
    String finalBlockNumber = "";
    for (int node = 0; node < simpleConfig.getNodes().size(); node++) {
      if (simpleConfig.getNodes().get(node).getPercentage() != 0) {
        finalBlockNumber =
            BallotAppUtil.getBlockNumber(simpleConfig.getNodes().get(node).getIp(), simpleConfig);
        logger.info(
            "Block number on "
                + simpleConfig.getNodes().get(node).getIp()
                + " after processing all transactions: "
                + finalBlockNumber);
      }
    }
  }

  public void distributeTransaction() {
    logger.debug("distributeTransaction enter");
    // distribute transaction to every node based on specified percentages
    int sumOfTx = 0;
    for (Node node : nodes) {
      int numOfTx = 0;
      // if percentage if > 0 make number of transactions at least 1
      if (node.getPercentage() != 0) {
        numOfTx = (int) (numberOfTransactions * (double) (node.getPercentage() / 100.0));
        numOfTx = Math.max(numOfTx, 1);
      }
      Map.Entry<String, Integer> entry =
          new AbstractMap.SimpleEntry<String, Integer>(node.getIp(), numOfTx);
      weightedEndpoints.add(entry);
      sumOfTx += numOfTx;
    }
    // if percentages do not divide evenly -> add the remaining tx to reach tx count
    int extra = numberOfTransactions - sumOfTx;

    int position = 0;
    while (extra > 0) {
      if (position == simpleConfig.getNodeCount()) {
        position = 0;
      }
      // only add to non-zero nodes
      if (nodes.get(position).getPercentage() != 0) {
        int newCount = weightedEndpoints.get(position).getValue() + 1;
        Map.Entry<String, Integer> entry =
            new AbstractMap.SimpleEntry<String, Integer>(nodes.get(position).getIp(), newCount);
        weightedEndpoints.set(position, entry);
        sumOfTx++;
        extra--;
      }
      position++;
    }

    BallotDApp.NUMBER = sumOfTx;
    logger.debug("distributeTransaction exit");
  }
}
