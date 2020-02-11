package ballotapp;

import bench.SimpleConfig;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.web3j.protocol.Web3j;
import org.web3j.protocol.core.DefaultBlockParameterName;
import org.web3j.protocol.core.methods.response.EthBlock;
import org.web3j.protocol.http.HttpService;

import java.io.IOException;

public class BallotAppUtil {
  private static final Logger logger = LogManager.getLogger(BallotAppUtil.class);

  public static String getBlockNumber(String server, SimpleConfig simpleConfig) throws IOException {

    if (simpleConfig.isHttp()) {
      server = "http://" + server + ":" + simpleConfig.getPort();
    } else {
      server = "https://" + server + ":" + simpleConfig.getPort();
    }
    logger.info(server);
    Web3j web3j = Web3j.build(new HttpService(server));
    EthBlock.Block block =
        web3j.ethGetBlockByNumber(DefaultBlockParameterName.LATEST, false).send().getBlock();
    return block.getNumber().toString();
  }
}
