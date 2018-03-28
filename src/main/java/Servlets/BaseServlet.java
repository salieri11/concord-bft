package Servlets;

import com.vmware.athena.*;

import connections.AthenaConnectionPool;
import connections.IAthenaConnection;

import java.io.IOException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.simple.JSONObject;

public abstract class BaseServlet extends HttpServlet {
   protected static final long serialVersionUID = 1L;

   protected abstract void doGet(final HttpServletRequest request,
                                 final HttpServletResponse response)
                           throws IOException;
   
   protected abstract JSONObject parseToJSON(
            Athena.AthenaResponse athenaResponse);

   protected void processGet(Athena.AthenaRequest req,
                              HttpServletResponse response) {
      JSONObject blocksListResponse = null;
      IAthenaConnection conn = null;
      Athena.AthenaResponse athenaResponse = null;
      try {
         conn = AthenaConnectionPool.getInstance().getConnection();
         boolean res = AthenaHelper.sendToAthena(req, conn);
         if (!res) {
            AthenaHelper.processResponse(response,
                           "Communication error",
                           HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
            return;
         }

         // receive response from Athena
         athenaResponse = AthenaHelper.receiveFromAthena(conn);
         if (athenaResponse == null) {
            AthenaHelper.processResponse(response, "Data error",
                  HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
            return;
         }
      } catch (Exception e) {
         AthenaHelper.processResponse(response, "Internal error",
               HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
         return;
      } finally {
         AthenaConnectionPool.getInstance().putConnection(conn);
      }

      blocksListResponse = parseToJSON(athenaResponse);
      String json = blocksListResponse == null ? null
            : blocksListResponse.toJSONString();
      AthenaHelper.processResponse(response, json,
            json == null ? HttpServletResponse.SC_INTERNAL_SERVER_ERROR
                  : HttpServletResponse.SC_OK);
   }
}
