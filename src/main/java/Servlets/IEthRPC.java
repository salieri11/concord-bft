package Servlets;

import com.vmware.athena.Athena;
import org.json.simple.JSONArray;
import com.vmware.athena.Athena.AthenaRequest;
import com.vmware.athena.Athena.AthenaResponse;
import org.json.simple.JSONObject;

public interface IEthRPC {

   Athena.EthRequest buildRequest(JSONArray params) throws Exception;

   JSONObject buildResponse(Athena.EthResponse ethResponse, JSONArray params);

}
