package Servlets;

import java.io.IOException;
import java.util.Iterator;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import Contracts.Compiler;
import Contracts.Contract;
import Contracts.ContractRegistryManager;
import org.apache.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONAware;

import com.vmware.athena.Athena;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

public class ContractsServlet extends BaseServlet {

   private static final long serialVersionUID = 1L;
   private final static Logger logger = Logger.getLogger(ContractsServlet.class);
   private final String jsonRpc = _conf.getStringValue("JSONRPC");
   
    /**
     * Handles the GET request for `/api/athena/contracts/<address>` API. GET
     * request returns the solidity code, bytecode and metadata information
     * of that contract
     * @param request
     * @param response
     * @throws IOException
     */
   @Override
   protected void doGet(final HttpServletRequest request,
                        final HttpServletResponse response) throws IOException {
       String uri = request.getRequestURI();
       int respStatus = HttpServletResponse.SC_OK;
       JSONObject responseJSON = new JSONObject();
       // Allow trailing /
       if (uri.charAt(uri.length() - 1) == '/') {
           uri = uri.substring(0, uri.length() - 1);
       }
    
       // look at the last token in URI after last `/`. If that token is a
       // contract address (starts with `0x`) then return information about that
       // specific address otherwise return information about all contracts
       String hash = uri.substring(uri.lastIndexOf('/') + 1);
       ContractRegistryManager crm = ContractRegistryManager.getInstance();
       
       if (Contract.isValidContractAddress(hash) && crm.hasContract(hash)) {
           Contract c = crm.getContract(hash);
           responseJSON.put("result", c.toJSON());
       } else if (hash.equals("contracts")) {
           // make a list of all contracts
           JSONArray jsonArray = new JSONArray();
           Iterator<Contract> cit = crm.allContracts();
           while (cit.hasNext()) {
               JSONObject contractJSON = new JSONObject();
               Contract c = cit.next();
               contractJSON.put("name", c.getName());
               contractJSON.put("address", c.getAddress());
               jsonArray.add(contractJSON);
           }
           responseJSON.put("result", jsonArray);
       } else {
           // throw error
           responseJSON.put("error", "Requested resource not found!");
           respStatus = HttpServletResponse.SC_NOT_FOUND;
       }
       
       processResponse(response, responseJSON.toJSONString(), respStatus, logger);
   }
    
    /**
     *
     * @return
     */
    /**

     * @param requestParams request json object
     * @param byteCode bytecode of the compiled contract
     * @return
     */
    /**
     * Builds a JSONObject which represents a request of type
     * `eth_sendTransaction` (Ethereum send transaction)
     * @param from Address from which new contract creation request was sent
     * @param id Request id
     * @param byteCode bytecode of compiled contract
     * @return JSON object of ethereum request
     */
   private JSONObject buildEthSendTxRequest(String from, long id,
                                            String byteCode) {
       // Now make a eth_sendTransaction request json object
       JSONObject ethRequest = new JSONObject();
       JSONArray paramsArray = new JSONArray();
       JSONObject transaction = new JSONObject();
       ethRequest.put("id", id);
       ethRequest.put("jsonrpc", jsonRpc);
       ethRequest.put("method", "eth_sendTransaction");
       transaction.put("from", from);
       transaction.put("data", byteCode);
       paramsArray.add(transaction);
       ethRequest.put("params", paramsArray);
       return ethRequest;
   }
    
    /**
     * Builds an Ethereum Get Transaction Receipt (eth_getTransactionReceipt)
     * request JSON object.
     * @param id The request id
     * @param transactionHash The hash of transction whose receipt is needed
     * @return the JSON request object
     */
    private JSONObject buildEthTxReceiptRequest(long id,
                                                String transactionHash) {
        // Now make a eth_sendTransaction request json object
        JSONObject ethRequest = new JSONObject();
        JSONArray paramsArray = new JSONArray();
        ethRequest.put("id", id);
        ethRequest.put("jsonrpc", jsonRpc);
        ethRequest.put("method", "eth_getTransactionReceipt");
        paramsArray.add(transactionHash);
        ethRequest.put("params", paramsArray);
        return ethRequest;
    }
    
    private String extractTransactionHash(String jsonStr) throws
            ParseException {
        JSONParser parser = new JSONParser();
        JSONObject json = (JSONObject) parser.parse(jsonStr);
        if (json.containsKey("error")) {
            return null;
        }
        return (String) json.get("result");
    }
   
    
    private String extractContractAddress(String jsonStr) throws
            ParseException {
        JSONParser parser = new JSONParser();
        JSONObject json = (JSONObject) parser.parse(jsonStr);
        JSONObject result = (JSONObject) json.get("result");
        String address = (String) result.get("contractAddress");
        return address;
    }
    
    
    /**
     * Handles the POST request for `/api/athena/contracts` API. A POST request
     * expects a solidity code as a part of the POST body. The POST body must
     * be a JSON having a key "params" and value being a strings of solidity contract code.
     * Example:
     * {
     *     "jsonrpc": "2.0",
     *     "params": "contract A {function() {return 10}}"
     * }
     * Every single of these contract codes are compiled and deployed to
     * athena
     * @param request
     * @param response
     * @throws IOException
     */
   protected void
             doPost(final HttpServletRequest request,
                    final HttpServletResponse response) throws IOException {
       String responseString;
       long id = -1;
       int respStatus = HttpServletResponse.SC_OK;
       try {
           String paramString
                   = request.getReader()
                   .lines()
                   .collect(Collectors.joining(System.lineSeparator()));
           logger.debug(paramString);
           JSONParser parser = new JSONParser();
           JSONObject requestObject = (JSONObject) parser.parse(paramString);
           // First extract some fields from requestParams
           String from = (String) requestObject.get("from");
           id = (long) requestObject.get("id");
           String solidityCode = (String) requestObject.get("code");

           // Compile the given solidity code
           Compiler.Result result = Compiler.compile(solidityCode);
           logger.debug(result);
           
           if (result.isSuccess()) {
               if (result.getByteCodeMap().size() > 1) {
                   logger.warn("Multiple contracts in single file submitted. " +
                           "Only deploying one of them.");
               }
               // TODO: Since we can't handle multiple contracts in same
               // file as of now, here if our source code contains multiple
               // contracts this object will hold the source code of all
               // contracts but will have bytecode and metadata of only one
               // of them. This needs to be fixed somehow.
               
               // Take out first contract code from byteCodeMap and deploy that
               String contractName = result.getByteCodeMap().keySet()
                       .iterator().next();
               logger.debug("Deploying contract: " + contractName);
               // Since EthDispatcher already has the code of handling ethereum
               // requests we just build a JSON object representing an ethereum
               // request (as if it was received like a normal ethereum JSON RPC
               // and forward it to EthDispatcher.
               // create eth_sendTransaction request for athena
               JSONObject sendTxrequest = buildEthSendTxRequest(from, id,
                       result.getByteCodeMap().get(contractName));
               responseString = new EthDispatcher().dispatch(response,
                       sendTxrequest);
               // Extract transaction hash from response string
               String transactionHash = extractTransactionHash(responseString);
               if (transactionHash != null) {
                   // If transactionHash == null then there was error from
                   // athena, forward it to client as it is
                   
                   // Now call eth_getTransactionReceipt API to get the address
                   // of deployed contract
                   JSONObject txReceiptRequest = buildEthTxReceiptRequest(id,
                           transactionHash);
                   String txReceipt = new EthDispatcher().dispatch
                           (response,
                           txReceiptRequest);
                   logger.info("New contract deployed at: " +
                           extractContractAddress(txReceipt));
                   
                   // Store contract
                   ContractRegistryManager crm = ContractRegistryManager
                           .getInstance();

                   // TODO ideally this parsing should never fail since metadata
                   // json is generated by solidity compiler, but if this
                   // parsing fails then the exception thrown will return a
                   // wrong error message to client. Fix this later.
                   JSONObject metadata = (JSONObject) (new JSONParser()).parse
                           (result.getMetadataMap().get(contractName));
                   
                   Contract newContract = new Contract(
                           contractName,
                           result.getByteCodeMap().get(contractName),
                           solidityCode,
                           metadata,
                           extractContractAddress(txReceipt));
                   crm.addNewContract(newContract);
               }
           } else {
               responseString = APIHelper.errorMessage("Compilation failure:\n" + result
                       .getStderr(), id, jsonRpc);
           }
           // TODO: The Compiler code is written to handle the case when there
           // are multiple contracts inside a single file. However, the
           // athena and EthSendTxHandler code is not written to handle
           // multiple contract creation request. Here, in Compiler.Result
           // object we have a Map of all contracts that need to be created so
           // for now we will just send athena request for one of the Contract
           // TODO: Store the address, contract bytecode and contract
           // metadata in helen DB
           // TODO: Figure out how to provide support for calling a particular
           // method of a contract
       } catch (ParseException pe) {
           logger.warn("Parsing exception while parsing request JSON", pe);
           responseString = APIHelper.errorMessage("Unable to parse " +
                           "request", id, jsonRpc);
           respStatus = HttpServletResponse.SC_BAD_REQUEST;
       } catch (Exception e) {
           logger.warn("Exception in request processing", e);
           responseString = APIHelper.errorMessage(e.getMessage(), id,
                   jsonRpc);
           respStatus = HttpServletResponse.SC_INTERNAL_SERVER_ERROR;
       }
       logger.debug("Response: " + responseString);
       processResponse(response,
               responseString,
               respStatus,
               logger);
   }

   /**
    * This method is not required in this servlet since we never directly
    * contact athena here. We only forward request to other servlets which
    * talk with athena.
    *
    * @param athenaResponse
    *           Protocol Buffer object containing Athena's reponse
    * @return Response in JSON format
    */
   @SuppressWarnings("unchecked")
   @Override
   protected JSONAware parseToJSON(Athena.AthenaResponse athenaResponse)
           throws UnsupportedOperationException {
      throw new UnsupportedOperationException("parseToJSON is not supported " +
              "in " + "ContractsServlet");
   }
}
