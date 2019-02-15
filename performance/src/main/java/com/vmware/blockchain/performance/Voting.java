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
import org.web3j.protocol.core.DefaultBlockParameter;
import org.web3j.protocol.core.DefaultBlockParameterName;
import org.web3j.protocol.http.HttpService;
import org.web3j.tx.Contract;
import org.web3j.tx.ManagedTransaction;
import org.web3j.tx.RawTransactionManager;
import org.web3j.utils.Numeric;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collections;
import java.util.Random;
import java.util.concurrent.Callable;

public class Voting implements Callable<long[]> {

   private Ballot ballot;
   private Web3j web3j;

   private Credentials credentials;

   private String signedMsg;

   private BigInteger proposal;


   public Voting(Web3j web3j, Ballot ballot, Credentials credentials) {
      this.web3j = web3j;
      this.ballot = ballot;
      this.credentials = credentials;
      Random random = new Random(System.currentTimeMillis());
      int index = random.nextInt(2);
      proposal = BigInteger.valueOf(index);
   }

   public Voting(String path, Credentials credentials) throws Exception {
      this.web3j = Web3j.build(new HttpService(BallotDApp.ENDPOINT));
      this.credentials = credentials;
      this.ballot = Utils.loadContract(web3j, path, credentials);
   }

   @Override
   public long[] call() throws Exception {
      long[] results = new long[2];
      //String data = encode(BigInteger.valueOf(index));
      results[0] = System.nanoTime();
      web3j.ethSendRawTransaction(signedMsg).send();
      //ballot.vote(BigInteger.valueOf(index)).send();
      results[1] = System.nanoTime();
      return results;
   }

   public String encode(BigInteger proposal) {
      final Function function = new Function(
            Ballot.FUNC_VOTE,
            Arrays.<Type>asList(new org.web3j.abi.datatypes.generated.Uint256(proposal)),
            Collections.<TypeReference<?>>emptyList());
      return FunctionEncoder.encode(function);
   }

   public String sign(String data) throws Exception{
      BigInteger nounce = web3j.ethGetTransactionCount(credentials.getAddress(), DefaultBlockParameterName.PENDING).send().getTransactionCount();
      RawTransaction rawTransaction = RawTransaction.createTransaction(nounce,
            ManagedTransaction.GAS_PRICE,
            Contract.GAS_LIMIT,
            ballot.getContractAddress(),
            BigInteger.valueOf(0),
            data);

      byte[] signedMsg = TransactionEncoder.signMessage(rawTransaction, credentials);
      String hexValue = Numeric.toHexString(signedMsg);
      return hexValue;
   }

   public void setSignedMsg(String message) {
      this.signedMsg = message;
   }

   public BigInteger getProposal() {
      return proposal;
   }

}
