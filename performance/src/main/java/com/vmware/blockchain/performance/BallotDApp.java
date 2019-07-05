package com.vmware.blockchain.performance;

import com.vmware.blockchain.samples.Ballot;
import com.vmware.blockchain.performance.BlockchainCredentials;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.web3j.abi.datatypes.Int;
import org.web3j.crypto.Credentials;
import org.web3j.crypto.ECKeyPair;
import org.web3j.crypto.Keys;
import org.web3j.crypto.WalletUtils;
import org.web3j.protocol.Web3j;
import org.web3j.protocol.core.methods.response.TransactionReceipt;
import org.web3j.protocol.http.HttpService;
import org.web3j.tx.Contract;
import org.web3j.tx.ManagedTransaction;

import java.io.*;
import java.security.PublicKey;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.Scanner;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

public class BallotDApp {
    private static final Logger log = LoggerFactory.getLogger(BallotDApp.class);
    public static String ENDPOINT = "http://10.193.11.107:8080/api/concord/eth";
    public static final String PASSWORD = "Test123456";
    private static String PROPOSAL_DATA_PATH = "data/proposals";
    public static String RESULT_PATH = "result";
    public static String DEPLOYER_KEY_PATH = "data/deployer_keystore";
    private static final String VOTER_KEY_STORES = RESULT_PATH + "/voter_keystores";
    private static String PERFORMANCE_DATA = RESULT_PATH + "/performance_result.log";
    private static  String CONTRACT_DATA_PATH = RESULT_PATH + "/contract";
    public static int NUMBER = 1000;
    private static double response;
    public static boolean ENABLE_AUTH = true;
    private Web3j web3j;

    public static void main(String[] args){
        if (args.length > 0) {
            String helenIP = args[0];
            ENDPOINT = "http://" + helenIP + ":8080/api/concord/eth";

            NUMBER = Integer.valueOf(args[1]);

            String dataPath = args[2];
            PROPOSAL_DATA_PATH = dataPath;

            String resultPath = args[3];
            DEPLOYER_KEY_PATH = resultPath + "/deployer_keystore";
            File keystorePath = new File(DEPLOYER_KEY_PATH);
            if (!keystorePath.exists()) {
                keystorePath.mkdirs();
            }
            CONTRACT_DATA_PATH = resultPath + "/contract";
            PERFORMANCE_DATA = resultPath + "/performance_result.log";

            int i=0;
            while (i < args.length ) {
                if (args[i].equals("--noAuth")) {
                    ENABLE_AUTH = false;
                }
                i++;
            }
            log.info("Authentication Enabled: " + ENABLE_AUTH);
        }

        try {
            BallotDApp dapp = new BallotDApp();

            BallotDeployer deployer = new BallotDeployer();
            deployer.deploy(PROPOSAL_DATA_PATH, CONTRACT_DATA_PATH);

            Credentials[] credentials = dapp.generateCredentialsForVoter();
            response = deployer.grantRightToVote(credentials);

            dapp.processingVoting(credentials);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void processingVoting(Credentials[] credentials) throws Exception {

        List<Voting> votings = new ArrayList<>();
        for (Credentials credential : credentials){
            if (BallotDApp.ENABLE_AUTH) {
                HttpService httpServiceEth = new HttpService(BallotDApp.ENDPOINT);
                httpServiceEth.addHeader("Authorization", okhttp3.Credentials.basic(BlockchainCredentials.USER, BlockchainCredentials.KEY));
                web3j = Web3j.build(httpServiceEth);
            } else {
                web3j = Web3j.build(new HttpService(BallotDApp.ENDPOINT));
            }

            Ballot ballot = Utils.loadContract(web3j, CONTRACT_DATA_PATH, credential);
            Voting voting = new Voting(web3j, ballot, credential);
            String data = voting.encode(voting.getProposal());
            String signedMsg = voting.sign(data);
            voting.setSignedMsg(signedMsg);
            votings.add(voting);
        }

        PrintWriter writer = new PrintWriter(new FileWriter(PERFORMANCE_DATA));
        ExecutorService excutor = Executors.newFixedThreadPool(NUMBER);
        Queue<Future<long[]>> resultList = new LinkedList<>();
        long conStart = System.nanoTime();
        for (Voting voting : votings) {
            Future<long[]> result = excutor.submit(voting);
            resultList.offer(result);
        }
        long conEnd = System.nanoTime();
        log.info("Concurrency Start Time is: " + conStart);
        log.info("Concurrency End time is: " + conEnd);
        log.info("Concurrency Total Time is: " + (conEnd - conStart));
        writer.printf("Concurrency Total Time is: %d\n", conEnd - conStart);
        Thread.sleep(10000);

        long start = Long.MAX_VALUE;
        long end = Long.MIN_VALUE;
        long sum = 0;
        while (!resultList.isEmpty()) {
            Future<long[]> future = resultList.poll();
            try {
                if (future.isDone()){
                    long[] result = future.get();
                    long diff = result[1] - result[0];
                    writer.printf("%d\t%d\t%d\n", result[0], result[1], diff);
                    sum += diff;
                    start = Math.min(start, result[0]);
                    end = Math.max(end, result[1]);
                } else {
                    log.info("Not completed yet, check later");
                    resultList.offer(future);
                    Thread.sleep(5000);
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

        log.info("Average time response time: " + sum*1.0/NUMBER);
        log.info("Start time of processing Voting: " + start);
        log.info("End time of processing Voting: " + end);
        log.info("Total time for process: " + (end - start) + " nano seconds");
        log.info("Transaction Rate: " + NUMBER/((end - start)/1000000000.0));

        writer.printf("Response TIme: %.4f\n", response);
        writer.printf("Burst Response Time: %.4f\n", sum*1.0e-9/NUMBER);
        writer.printf("Transaction Rate: %.2f\n", NUMBER/((end - start)*1.0e-9));

        writer.close();

        excutor.shutdown();
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
