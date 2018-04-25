package Servlets;

import org.json.simple.JSONArray;
import com.vmware.athena.Athena.AthenaRequest;
import com.vmware.athena.Athena.AthenaResponse;

public interface IEthRPC {

   AthenaRequest handleRequest(Long id, String method,
                               JSONArray params) throws Exception;

   String buildResponse(AthenaResponse athenaResponse, String txHash, String method);

   String buildLocalResponse(Object data, Long id);
}
