package dappbench;

import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.constructor.Constructor;

import org.web3j.crypto.Credentials;

import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;

import java.io.*;

import org.apache.logging.log4j.*;

import com.vmware.blockchain.performance.*;

import java.text.SimpleDateFormat;

public class BenchMaster {
	private static final Logger logger = LogManager.getLogger(BenchMaster.class);

	public static void main(String[] args) {
		System.out.println("Starting DAppBench...");
		Yaml yaml = new Yaml(new Constructor(Benchmark.class));
		try {
			InputStream inputStream = new FileInputStream(new File(args[0]));
			Benchmark benchmark = yaml.load(inputStream);
			SimpleConfig simpleConfig = benchmark.getSimpleConfig();
			AdvancedConfig advancedConfig = benchmark.getAdvancedConfig();
			List<Workload> workloads = simpleConfig.getWorkloads();
			int sleepTime = simpleConfig.getSleepTime();
			int totalWorkloads = 0;

			String outputDir = simpleConfig.getOutputDir();

			File directory = new File(outputDir);
			if (! directory.exists()){
				directory.mkdir();
			}

			//generate a unique driverID for this run
			BallotDApp.DRIVERID = new Random().nextInt(999999);
			List<Node> nodes = benchmark.getSimpleConfig().getNodes();
			BallotDApp.PORT = simpleConfig.getPort();
			
			//advanced configuration
			BallotDApp.CONCORD_USERNAME = advancedConfig.getConcordUsername();
			BallotDApp.CONCORD_PASSWORD = advancedConfig.getConcordUsername();

			for (Workload workload : workloads) {
				if (workload.getDapp().equals("Ballot")) {
					List<Entry<String, Integer>> weightedEndpoints = new ArrayList<Entry<String, Integer>>();
					BallotDApp dapp = new BallotDApp();
					List<String> workloadParams = workload.getParams();

					if (workloadParams.get(1).equals("--helen")) {
						String helenIP = workloadParams.get(2);
						BallotDApp.ENDPOINT = "http://" + helenIP + ":8080/api/concord/eth";
						logger.debug("Connected to Helen at " + BallotDApp.ENDPOINT);
						BallotDApp.CONCORD = false;
					} else if (workloadParams.get(1).equals("--concord")) {
						String concordIP = workloadParams.get(2);
						BallotDApp.ENDPOINT = "https://" + concordIP + ":" + simpleConfig.getPort();
						int sumOfPercentages = 0;
						for (Node node : nodes) {
							sumOfPercentages += node.getPercentage();
						}
						if (sumOfPercentages != 100) {
							logger.info("The node distribution does not add up to 100%");
						}

						//distribute transaction to every node based on specified percentages
						int sumOfTx = 0;
						for (Node node : nodes) {
							int numOfTx = 0;
							//if percentage if > 0 make number of transactions at least 1
							if (node.getPercentage() != 0) {
								numOfTx = (int) (Integer.valueOf(workloadParams.get(3))*(double)(node.getPercentage()/100.0));
								numOfTx = Math.max(numOfTx, 1);
							}
							Map.Entry<String,Integer> entry =
									new AbstractMap.SimpleEntry<String, Integer>(node.getIp(), numOfTx);
							weightedEndpoints.add(entry);
							sumOfTx += numOfTx;
						}
						//if percentages do not divide evenly -> add the remaining tx to reach tx count
						int extra = Integer.valueOf(workloadParams.get(3))-sumOfTx;

						int position = 0;
						while (extra > 0) {
							if (position == benchmark.getSimpleConfig().getNodeCount()) {
								position = 0;
							}
							//only add to non-zero nodes
							if (nodes.get(position).percentage != 0) {
								int newCount = weightedEndpoints.get(position).getValue()+1;
								Map.Entry<String,Integer> entry =
										new AbstractMap.SimpleEntry<String, Integer>(nodes.get(position).getIp(), newCount);
								weightedEndpoints.set(position, entry);
								sumOfTx++;
								extra--;
							}
							position++;
						}

						BallotDApp.NUMBER = sumOfTx;

					} else {
						logger.error("Please specify the connection endpoint. (--helen or --concord as 1st arg)");
						return;
					}

					for (int i = 0; i < workload.getNumOfRuns(); i++) {
						BallotDApp.RUNID = new SimpleDateFormat("yyyyMMddHHmm").format(new Date());
						BallotDApp.RATE_CONTROL = workload.getRateControl();
						dapp.setEndpoints(weightedEndpoints);
						if (advancedConfig.getNumberThreads() != 0) {
							BallotDApp.NUMBER_THREADS = advancedConfig.getNumberThreads();
						} else {
							BallotDApp.NUMBER_THREADS = (Runtime.getRuntime().availableProcessors()*2);
						}
						String dataPath = workloadParams.get(4);
						if(workload.getLogging())
							BallotDApp.ENABLE_LOGGING = true;
						String PROPOSAL_DATA_PATH = dataPath;
						String resultPath = outputDir + "/workload" + totalWorkloads;
						BallotDApp.DEPLOYER_KEY_PATH = resultPath + "/deployer_keystore";
						File keystorePath = new File(BallotDApp.DEPLOYER_KEY_PATH);
						if (!keystorePath.exists()) {
							keystorePath.mkdirs();
						}
						dapp.setContractDataPath(resultPath + "/contract");
						dapp.setPerformanceData(resultPath + "/performance_result.log");
						dapp.setPerformanceDataCSV(resultPath + "/performance.csv");

						String filename = new SimpleDateFormat("yyyyMMddHHmm").format(new Date());
						BallotDApp.WAVEFRONT_DATA_PATH = resultPath + "/wavefront/" + filename + ".log";
						BallotDApp.STAT_DATA_PATH = resultPath + "/stat-" + filename + ".log";
						File wavefrontPath = new File(resultPath + "/wavefront/");
						if (!wavefrontPath.exists()) {
							wavefrontPath.mkdirs();
						}
						try {
							BallotDApp.CLIENT = Utils.getClient();
						} catch (Exception e) {
							e.printStackTrace();
						}
						try {
							BallotDeployer deployer = new BallotDeployer();
							deployer.deploy(PROPOSAL_DATA_PATH, dapp.getContractDataPath());

							Credentials[] credentials = dapp.getCredentials();
							deployer.grantRightToVote(credentials);
							dapp.process(credentials);

						} catch (Exception e) {
							e.printStackTrace();
						}
						System.gc();
						Thread.sleep(sleepTime);
						totalWorkloads++;
					}
				}
			}
			System.exit(1);
		} catch (FileNotFoundException e) {
			System.out.println(e.getMessage());
		} catch (InterruptedException e) {
			System.out.println(e.getMessage());
		} catch (Exception e) {
			System.out.println(e.getMessage());
		}
		System.out.println("Finish benchmark!");
	}

	public static String getExecutionLog(BufferedReader op, BufferedReader error, int exitVal) {
		String err = "";
		String line;
		try {
			while((line = error.readLine()) != null) {
				err = err + "\n" + line;
			}
		} catch (final IOException e) {
		}
		String output = "";
		try {
			while((line = op.readLine()) != null) {
				output = output + "\n" + line;
			}
		} catch (final IOException e) {
		}
		try {
			error.close();
			op.close();
		} catch (final IOException e) {
		}
		return "exitVal: " + exitVal + ", error: " + err + ", output: " + output;
	}
}
