/**
 * url endpoint : Any endpoint other than /api/members and /static/* 
 * 
 * This servlet is used to serve static content.
 */
package Servlets;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Properties;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.log4j.Logger;
import configurations.FileConfiguration;
import configurations.IConfiguration;

/**
 * Servlet class.
 */
public class DefaultContent extends HttpServlet {
   private static final long serialVersionUID = 1L;
   private static String defaultContentPath;
   private static final Logger logger = Logger.getLogger(StaticContent.class);

   public DefaultContent() throws IOException {
      IConfiguration c = FileConfiguration.getInstance();
      defaultContentPath = c.getStringValue("Server_DefaultResponse");
   }

   /**
    * Services a get request. Fetches the resource from the specified path and
    * returns it.
    * 
    * @param request
    *           The request received by the servlet
    * @param response
    *           The response object used to respond to the client
    */
   protected void doGet(final HttpServletRequest request,
            final HttpServletResponse response)
            throws ServletException, IOException {
      // read the file
      try (FileInputStream inputStream = new FileInputStream(
               defaultContentPath)) {
         // Set response header
         response.setHeader("Content-Transfer-Encoding", "UTF-8");
         // Set content type
         response.setContentType("text/html");

         int c;
         try {
            while ((c = inputStream.read()) != -1) {
               response.getWriter().write(c);
            }
         } catch (IOException e) {
            logger.error("Error in reading from tcp input stream");
            throw e;
         }
      } catch (FileNotFoundException e) {
         logger.error("File not found");
         throw e;
      }
   }
}