package Servlets.EthRPCHandlers;

import Servlets.APIHelper;
import Servlets.EthDispatcher;
import com.vmware.athena.Athena;
import com.vmware.athena.Athena.EthRequest;
import com.vmware.athena.Athena.EthRequest.EthMethod;
import com.vmware.athena.Athena.EthResponse;
import org.apache.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

public class EthGetStorageAtHandler extends AbstractEthRPCHandler {
    
    Logger logger = Logger.getLogger(EthGetStorageAtHandler.class);
    
    @Override
    public EthRequest buildRequest(JSONObject requestJson) throws Exception {
        Athena.EthRequest ethRequest = null;
        try {
            EthRequest.Builder b = initializeRequestObject(requestJson);
            b.setMethod(EthMethod.GET_STORAGE_AT);
            
            JSONArray params = extractRequestParams(requestJson);
            if (params == null) {
                logger.error("'params' not present");
                throw new EthRPCHandlerException(EthDispatcher.errorMessage("'params' not present",
                    b.getId(), jsonRpc));
            }
            b.setAddrTo(APIHelper.hexStringToBinary((String) params.get(0)));
            String p = (String) params.get(1);
            String s = APIHelper.padZeroes(p);
            b.setData(APIHelper.hexStringToBinary(s));
            
            ethRequest = b.build();
        } catch (Exception e) {
            logger.error(e.getMessage());
            throw e;
        }
        return ethRequest;
    }
    
    @SuppressWarnings("unchecked")
    @Override
    public JSONObject buildResponse(EthResponse ethResponse,
                                    JSONObject requestJson) {
        JSONObject respObject = initializeResponseObject(ethResponse);
        respObject.put("result",
            APIHelper.binaryStringToHex(ethResponse.getData()));
        return respObject;
    }
}
