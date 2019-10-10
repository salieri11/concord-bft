package dappbench;

import static java.lang.ProcessBuilder.Redirect.INHERIT;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.sun.jersey.api.client.Client;
import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.WebResource;
import com.vmware.blockchain.performance.Utils;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Random;

public class DAMLManager {
    private final static Logger logger = LoggerFactory.getLogger(DAMLManager.class);

    private String contractPath;
    private String executable;
    private String ledgerHost;
    private int ledgerPort;
    private String party;
    private int restPort;
    private int rateControl;
    private int numOfTransactions;
    private String contractFile;
    public boolean deployLedgerRequired;
    private String endPoint;

    public DAMLManager(Workload workload) {

        List<String> params = workload.getParams();
        this.contractPath = params.get(0);
        this.executable = params.get(1);
        this.ledgerHost = params.get(2);
        this.ledgerPort = Integer.parseInt(params.get(3));
        this.party = params.get(4);
        this.restPort = Integer.parseInt(params.get(5));
        this.numOfTransactions = Integer.parseInt(params.get(6));
        this.rateControl = workload.getRateControl();
        this.deployLedgerRequired = Boolean.parseBoolean(workload.getParams().get(7));
        this.contractFile = params.get(8);
        this.endPoint = params.get(9);
        logger.info("Number of Transactions: " + numOfTransactions);
        logger.info("Rate control value: " + rateControl);
    }

    public boolean deployLedgerRequired() {
        return deployLedgerRequired;
    }

    public void deployToLedger() {
        // Calling an external process to deploy the daml app to the ledger
        logger.info("Deploying contracts");
        try {
            // Calling an external process to deploy the daml app to the ledger
            String deploymentCommand = "daml deploy --host=" + ledgerHost + " --port=" + ledgerPort;
            logger.info("Deployment command: " + deploymentCommand);

            // Running the above command
            Runtime run = Runtime.getRuntime();
            Process deploymentProc = run.exec(deploymentCommand, null, new File(contractPath));
            deploymentProc.waitFor();
            BufferedReader b = new BufferedReader(new InputStreamReader(deploymentProc.getInputStream()));
            String line;
            while ((line = b.readLine()) != null) {
                logger.info(line);
            }
            b.close();
        } catch (IOException | InterruptedException e) {
            e.printStackTrace();
        }
    }

    public void startService() {
        //Starting daml backend service
        logger.info("Starting service");
        try {
            String startServiceCommend = "java -jar " + executable + " " + ledgerHost + " " + ledgerPort
                    + " " + party + " " + restPort;

            logger.info("start service command: " + startServiceCommend);
            
            Process process = new ProcessBuilder().command(startServiceCommend.split("\\s+")).directory(new File(contractPath))
            		                              .redirectOutput(INHERIT).redirectErrorStream(true)
            		                              .start();
            Runtime.getRuntime().addShutdownHook( new Thread(() -> process.destroyForcibly()));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void processDAMLTransactions () {
        try
        {
            //JSON parser object to parse read file
            String jsonContract = contractPath + "/" + contractFile;
            HashMap<String,Object> contractMap =
                    new ObjectMapper().readValue(new File(jsonContract), HashMap.class);


            logger.info("Starting transaction ...");
            // Initiate REST API call
            // Given the rest endpoint

            String uri = "http://localhost:" + restPort + "/" + endPoint;

            logger.info("uri: " + uri);
            Client client = Client.create();

            WebResource webResource = client.resource(uri);
            Random random = new Random();
            long startTime = System.nanoTime();
            for (int i = 0; i < numOfTransactions; i++ ) {
                int iouAmount = random.nextInt(10000);
                contractMap.put("amount", iouAmount);
                JSONObject json = new JSONObject(contractMap);
                String jsonContent = json.toString();

                logger.info("Json Contract: " + jsonContent);
                ClientResponse response = webResource.accept("application/json")
                        .put(ClientResponse.class, jsonContent);

                if (response.getStatus() != 200) {
                    logger.info("Failed : HTTP error code : "
                            + response.getStatus());
                    throw new RuntimeException("Failed : HTTP error code : "
                            + response.getStatus());
                }

                if(rateControl != 0) {
                    long timeToSleep = 1000_000_000 / rateControl;
                    Utils.applyRateControl(timeToSleep, i, startTime);
                }

            }
            long endTime = System.nanoTime();
            long totalTime = endTime - startTime;
            long timeElaps = totalTime / 1000000;
            logger.info("Total time taken: " + timeElaps + " ms");
            logger.info("Average latency: " + totalTime / (numOfTransactions * 1000000) + " ms");
            logger.info("Throughput: " + (numOfTransactions * 1.0 / totalTime) * 1000000000 + " tps");

        } catch (IOException e) {
            e.printStackTrace();
        }

    }
}
