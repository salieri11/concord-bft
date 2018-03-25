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
import configurations.FIleConfiguration;

/**
 * Servlet class.
 */
public class DefaultContent extends HttpServlet {
   private static final long serialVersionUID = 1L;
   private static Properties config;
   private static String defaultContentPath;
   private final Logger logger;

   public DefaultContent() throws IOException {
      logger = Logger.getLogger(StaticContent.class);

      // Read configurations
      FIleConfiguration s;
      try {
         s = FIleConfiguration.getInstance();
      } catch (IOException e) {
         logger.error("Error in reading configurations");
         throw new IOException();
      }
      config = s.configurations;
      defaultContentPath = config.getProperty("Server_DefaultResponse");
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
   @SuppressWarnings("resource")
   protected void doGet(final HttpServletRequest request,
            final HttpServletResponse response)
            throws ServletException, IOException {
      // read the file
      FileInputStream inputStream;
      try {
         inputStream = new FileInputStream(defaultContentPath);
      } catch (FileNotFoundException e1) {
         logger.error("File not found");
         throw new FileNotFoundException();
      }

      // Set response header
      response.setHeader("Content-Transfer-Encoding", "UTF-8");
      // Set content type
      response.setContentType("text/html");

      // Respond to client
      int c;
      try {
         while ((c = inputStream.read()) != -1) {
            response.getWriter().write(c);
         }
      } catch (IOException e) {
         logger.error("Error in reading from tcp input stream");
         throw new IOException();
      }

      // close resources
      if (inputStream != null) {
         try {
            inputStream.close();
         } catch (IOException e) {
            logger.error("Error in closing tcp input stream");
            throw new IOException();
         }
      }
      try {
         if (response.getWriter() != null) {
            response.getWriter().close();
         }
      } catch (IOException e) {
         logger.error("Error in closing the response writer object");
         throw new IOException();
      }
   }
}