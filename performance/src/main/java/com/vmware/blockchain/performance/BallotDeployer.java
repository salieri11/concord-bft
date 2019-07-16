package com.vmware.blockchain.performance;

import com.vmware.blockchain.samples.Ballot;
import com.vmware.blockchain.performance.BlockchainCredentials;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.web3j.crypto.Credentials;
import org.web3j.protocol.Web3j;
import org.web3j.protocol.core.methods.response.TransactionReceipt;
import org.web3j.protocol.http.HttpService;
import org.web3j.tx.Contract;
import org.web3j.tx.ManagedTransaction;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.util.List;

public class BallotDeployer {

   private static final Logger log = LoggerFactory.getLogger(BallotDApp.class);

   private Web3j web3j;
   private Credentials credentials;
   private Ballot ballot;

   public BallotDeployer() {

   }

   public void deploy(String proposalPath, String contractPath) throws Exception {
        // create the web3j interface to interact with Helen, here is the ENDPOINT is Ethereum JSON API endpoint defined in Helen
        if (BallotDApp.ENABLE_AUTH) {
            HttpService httpServiceEth = new HttpService(BallotDApp.ENDPOINT);
            httpServiceEth.addHeader("Authorization", okhttp3.Credentials.basic(BlockchainCredentials.USER, BlockchainCredentials.KEY));
            web3j = Web3j.build(httpServiceEth);
        } else {
            web3j = Web3j.build(new HttpService(BallotDApp.ENDPOINT));
        } 

      HttpService httpServiceEth = new HttpService(BallotDApp.ENDPOINT);
      httpServiceEth.addHeader("Authorization", okhttp3.Credentials.basic(BlockchainCredentials.USER, BlockchainCredentials.KEY));
      Web3j web3j = Web3j.build(httpServiceEth);

      log.info("Connected to Ethereum client version: "
              + web3j.web3ClientVersion().send().getWeb3ClientVersion());

      // load credential
      credentials = Utils.loadCredential(BallotDApp.PASSWORD, BallotDApp.DEPLOYER_KEY_PATH);

      List<byte[]> proposals = Utils.getProposals(proposalPath);

      try {
         ballot = Ballot.deploy(web3j, credentials, ManagedTransaction.GAS_PRICE, Contract.GAS_LIMIT, proposals).send();
      } catch (Exception e) {
         log.info(e.getMessage());
      }


      String contractAddress = ballot.getContractAddress();
      log.info("Smart contract deployed to address " + contractAddress);

      BufferedWriter writer = new BufferedWriter(new FileWriter(new File(contractPath)));
      writer.write(contractAddress);
      writer.close();

      TransactionReceipt receipt = ballot.getTransactionReceipt().get();
      log.info("Transaction info about deploying contract:\n" + receipt);
   }

   public double grantRightToVote(Credentials[] credentials) throws Exception {
      long sum = 0;
      for (Credentials credential : credentials) {
         long start = System.nanoTime();
         tryGrantRight(credential);
         long end = System.nanoTime();
         sum += end - start;
      }
      log.info("Total Time for Granting Right is " + sum + " nano seconds");
      return sum *(1.0e-9) / BallotDApp.NUMBER;
   }

   public void tryGrantRight(Credentials credentials) {
      try {
         ballot.giveRightToVote(credentials.getAddress()).send();
      } catch (Exception e) {
         tryGrantRight(credentials);
      }
   }
}
