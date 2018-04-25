/**
 * This is the base class for all API based servlets.
 */
package Servlets;

import com.vmware.athena.*;
import connections.AthenaConnectionPool;
import connections.IAthenaConnection;
import configurations.ConfigurationFactory;
import configurations.ConfigurationFactory.ConfigurationType;
import configurations.IConfiguration;
import java.io.IOException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.log4j.Logger;
import org.json.simple.JSONAware;

public abstract class BaseServlet extends HttpServlet {
   protected static final long serialVersionUID = 1L;

   protected abstract void
             doGet(final HttpServletRequest request,
                   final HttpServletResponse response) throws IOException;

   protected IConfiguration _conf;

   protected abstract JSONAware
             parseToJSON(Athena.AthenaResponse athenaResponse);

   protected BaseServlet() {
      _conf = ConfigurationFactory.getConfiguration(ConfigurationType.File);
   }

   /**
    * Process get request
    *
    * @param req
    *           - Athena request object
    * @param response
    *           - HTTP servlet response object
    * @param log
    *           - specifies logger from servlet to use
    */
   protected void processGet(Athena.AthenaRequest req,
                             HttpServletResponse response, Logger log) {
      IAthenaConnection conn = null;
      Athena.AthenaResponse athenaResponse = null;
      try {
         conn = AthenaConnectionPool.getInstance().getConnection();
         if (conn == null) {
            processResponse(response,
                            "Connection error",
                            HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
                            log);
            return;
         }

         boolean res = AthenaHelper.sendToAthena(req, conn, _conf);
         if (!res) {
            processResponse(response,
                            "Communication error",
                            HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
                            log);
            return;
         }

         // receive response from Athena
         athenaResponse = AthenaHelper.receiveFromAthena(conn);
         if (athenaResponse == null) {
            processResponse(response,
                            "Data error",
                            HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
                            log);
            return;
         }
      } catch (Exception e) {
         processResponse(response,
                         "Internal error",
                         HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
                         log);
         return;
      } finally {
         AthenaConnectionPool.getInstance().putConnection(conn);
      }

      JSONAware respObject = parseToJSON(athenaResponse);
      String json = respObject == null ? null : respObject.toJSONString();

      processResponse(response,
                      json,
                      json == null
                         ? HttpServletResponse.SC_INTERNAL_SERVER_ERROR
                         : HttpServletResponse.SC_OK,
                      log);
   }

   /**
    * Process response back to the client
    *
    * @param resp
    *           - response object from the servlet
    * @param data
    *           - response data, JSON
    * @param status
    *           - HTTP response status
    * @param log
    *           - servlet specific logger to use
    */
   protected void processResponse(HttpServletResponse resp, String data,
                                  int status, Logger log) {
      try {
         // Set client response header
         resp.setHeader("Content-Transfer-Encoding", "UTF-8");
         resp.setContentType("application/json");
         resp.setStatus(status);
         if (data != null)
            resp.getWriter().write(data);
      } catch (Exception e) {
         log.error("processResponse", e);
      }
   }
}
