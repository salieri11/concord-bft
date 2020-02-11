package ballotapp;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.web3j.abi.FunctionEncoder;
import org.web3j.abi.TypeReference;
import org.web3j.abi.datatypes.Function;
import org.web3j.abi.datatypes.Type;
import org.web3j.crypto.Credentials;
import org.web3j.crypto.RawTransaction;
import org.web3j.crypto.TransactionEncoder;
import org.web3j.protocol.Web3j;
import org.web3j.protocol.core.DefaultBlockParameterName;
import org.web3j.protocol.core.methods.response.EthGetBalance;
import org.web3j.protocol.core.methods.response.EthGetTransactionReceipt;
import org.web3j.protocol.core.methods.response.EthSendTransaction;
import org.web3j.tx.gas.DefaultGasProvider;
import org.web3j.utils.Numeric;
import samples.Ballot;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collections;
import java.util.concurrent.Callable;

public class RightGranter implements Callable<EthGetTransactionReceipt> {
  private static final Logger log = LoggerFactory.getLogger(BallotDApp.class);
  private Ballot ballot;
  private Web3j web3j;

  // POLLING_INTERVAL is the number of milliseconds in which the thread
  // checks if the transaction it sent has been mined
  // private static int POLLING_INTERVAL = 5;

  private Credentials credentials;

  private String signedMsg;

  private String data;

  private Function function;

  public RightGranter(Web3j web3j, Ballot ballot, Credentials credentials) {
    this.web3j = web3j;
    this.ballot = ballot;
    this.credentials = credentials;
  }

  @SuppressWarnings("rawtypes")
  public String encode() {
    final Function _function =
        new Function(
            Ballot.FUNC_GIVERIGHTTOVOTE,
            Arrays.<Type>asList(new org.web3j.abi.datatypes.Address(this.credentials.getAddress())),
            Collections.<TypeReference<?>>emptyList());

    data = FunctionEncoder.encode(_function);
    this.function = _function;
    return data;
  }

  public String sign(BigInteger nonce) throws Exception {
    RawTransaction rawTransaction =
        RawTransaction.createTransaction(
            nonce,
            BallotDApp.GAS_PRICE,
            DefaultGasProvider.GAS_LIMIT,
            ballot.getContractAddress(),
            BigInteger.valueOf(0),
            data);
    log.info("RT");
    log.info("Nonce = " + rawTransaction.getNonce());
    log.info("Gas price = " + rawTransaction.getGasPrice().toString());
    log.info("Gas limit = " + rawTransaction.getGasLimit().toString());
    log.info("To = " + rawTransaction.getTo());
    log.info("Value = " + rawTransaction.getValue());
    log.info("Data = " + rawTransaction.getData());
    EthGetBalance ethGetBalance =
        web3j
            .ethGetBalance(this.credentials.getAddress(), DefaultBlockParameterName.PENDING)
            .sendAsync()
            .get();
    log.info("Balance = " + ethGetBalance.getBalance());
    log.info("Account address = " + this.credentials.getAddress());
    log.info("Function = " + function.getInputParameters() + ", " + function.getOutputParameters());
    byte[] signedMsg =
        TransactionEncoder.signMessage(rawTransaction, BallotDeployer.getChairperson());
    String hexValue = Numeric.toHexString(signedMsg);
    this.signedMsg = hexValue;
    return hexValue;
  }

  @Override
  public EthGetTransactionReceipt call() throws Exception {
    EthSendTransaction ethGrantVotingRightsTransaction =
        web3j.ethSendRawTransaction(signedMsg).sendAsync().get();
    log.info("EST");
    log.info("ID = " + ethGrantVotingRightsTransaction.getId());
    log.info("JSON RPC = " + ethGrantVotingRightsTransaction.getJsonrpc());
    log.info("Result = " + ethGrantVotingRightsTransaction.getResult());
    log.info("Raw response = " + ethGrantVotingRightsTransaction.getRawResponse());
    String transactionHash = ethGrantVotingRightsTransaction.getTransactionHash();
    log.info("TxHash = " + transactionHash);

    EthGetTransactionReceipt transactionReceipt;
    while (true) {
      transactionReceipt = web3j.ethGetTransactionReceipt(transactionHash).send();
      if (transactionReceipt.getTransactionReceipt().isPresent()) {
        break;
      }
      Thread.sleep(10000);
    }
    log.info("Right granter thread exiting.");
    return transactionReceipt;
  }
}
