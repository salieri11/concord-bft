package Servlets.EthRPCHandlers;

import Servlets.APIHelper;
import Servlets.EthDispatcher;
import com.vmware.athena.Athena;
import configurations.ConfigurationFactory;
import configurations.ConfigurationFactory.ConfigurationType;
import configurations.IConfiguration;
import connections.AthenaConnectionPool;
import connections.IAthenaConnection;
import org.apache.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;

public class EthLocalResponseHandler extends AbstractEthRPCHandler {

   private static Logger logger = Logger.getLogger(EthFilterHandler.class);
   private String jsonRpc;
   private IConfiguration _conf;

   public EthLocalResponseHandler() {
      _conf = ConfigurationFactory.getConfiguration(ConfigurationType.File);
      jsonRpc = _conf.getStringValue("JSONRPC");
   }

   public Athena.EthRequest
          buildRequest(JSONObject requestJson) throws Exception {
      return null;
   }

   /**
    * Initializes response object by using request JSONObject for local methods.
    * An overload of this method which accepts EthResponse as input parameter is
    * defined in {@link AbstractEthRPCHandler} class. However, for requests
    * which can be handled locally we do not even call athena and hence do not
    * have a valid athena response. Hence we can not use the method provided by
    * parent class.
    */
   @SuppressWarnings("unchecked")
   JSONObject initializeResponseObject(JSONObject requestJson) {
      JSONObject respObject = new JSONObject();
      respObject.put("id", requestJson.get("id"));
      respObject.put("jsonrpc", _conf.getStringValue("JSONRPC"));
      return respObject;
   }

   @SuppressWarnings("unchecked")
   public JSONObject buildResponse(Athena.EthResponse ethResponse,
                                   JSONObject requestJson) throws Exception {
      long id = (long) requestJson.get("id");
      String ethMethodName = EthDispatcher.getEthMethodName(requestJson);
      JSONArray params = extractRequestParams(requestJson);
      /*
       * Here we can not use parents initializeResponseObject method because it
       * takes a valid EthResponse object as input parameter, however in local
       * methods we do not even call athena and hence do not have a valid value
       * for that parameter. Instead we provide our own overload of
       * initializeResponseObject method in this class which initializes object
       * with requestJson object.
       */
      JSONObject result = initializeResponseObject(requestJson);

      Object localData = null;

      if (ethMethodName.equals(_conf.getStringValue("Web3SHA3_Name"))) {
         // Request should contain just one param value
         if (params.size() != 1) {
            logger.error("Invalid request parameter : params");
            throw new EthRPCHandlerException(EthDispatcher.errorMessage("'params' must contain only one element",
                                                                        id,
                                                                        jsonRpc));
         }

         try {
            localData = APIHelper.getKeccak256Hash((String) params.get(0));
            logger.info("Generated keccak hash is: " + localData);
         } catch (Exception e) {
            logger.error("Error in calculating Keccak hash", e);
            throw new EthRPCHandlerException(EthDispatcher.errorMessage("'invalid param",
                                                                        id,
                                                                        jsonRpc));
         }
      } else if (ethMethodName.equals(_conf.getStringValue("RPCModules_Name"))) {
         JSONParser p = new JSONParser();
         localData
            = (((JSONArray) p.parse(_conf.getStringValue("RPCModules"))).get(0));
      } else if (ethMethodName.equals(_conf.getStringValue("Coinbase_Name"))) {
         localData = _conf.getStringValue("Coinbase");
      } else if (ethMethodName.equals(_conf.getStringValue("ClientVersion_Name"))) {
         localData = _conf.getStringValue("ClientVersion");
      } else if (ethMethodName.equals(_conf.getStringValue("Mining_Name"))) {
         localData = _conf.getIntegerValue("Is_Mining") == 0 ? false : true;
      } else if (ethMethodName.equals(_conf.getStringValue("NetVersion_Name"))) {
         if (!EthDispatcher.netVersionSet) {
            // The act of creating a connection retrieves info about athena.
            IAthenaConnection conn = null;
            try {
               conn = AthenaConnectionPool.getInstance().getConnection();
               conn.check();
            } catch (IllegalStateException | InterruptedException e) {
               e.printStackTrace();
            } catch (Exception e) {
               logger.error("Unable to connect to athena.");
               throw new EthRPCHandlerException(EthDispatcher.errorMessage("Unable to connect to athena.",
                                                                           id,
                                                                           jsonRpc));
            }
         } else {
            EthDispatcher.netVersionSet = true;
         }
         localData = EthDispatcher.netVersion;
      } else if (ethMethodName.equals(_conf.getStringValue("Accounts_Name"))) {
         JSONArray usersJsonArr = new JSONArray();
         String usersStr = _conf.getStringValue("USERS");
         if (usersStr != null && !usersStr.trim().isEmpty()) {
            String[] usersArr = usersStr.split(",");
            for (int i = 0; i < usersArr.length; i++) {
               usersJsonArr.add(usersArr[i]);
            }
         }
         localData = usersJsonArr;
      }

      result.put("result", localData);

      return result;
   }

}
