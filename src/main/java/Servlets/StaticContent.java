/**
 * url endpoint : /static/*
 * 
 * This servlet is used to serve static content.
 */
package Servlets;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Properties;

import javax.servlet.DispatcherType;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Logger;
import org.apache.tika.exception.TikaException;
import org.apache.tika.metadata.HttpHeaders;
import org.apache.tika.metadata.Metadata;
import org.apache.tika.metadata.TikaMetadataKeys;
import org.apache.tika.mime.MediaType;
import org.apache.tika.parser.AutoDetectParser;
import org.apache.tika.parser.ParseContext;
import org.apache.tika.parser.Parser;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;
import configurations.FileConfiguration;
import configurations.IConfiguration;
import io.undertow.util.CanonicalPathUtils;
import io.undertow.util.StatusCodes;

/**
 * Servlet class.
 */
public class StaticContent extends HttpServlet {
   private static final long serialVersionUID = 1L;
   private static String staticContentFolder;
   private static char separatorChar;
   private static final Logger logger = Logger.getLogger(StaticContent.class);

   public StaticContent() throws IOException {
      IConfiguration config = FileConfiguration.getInstance();
      staticContentFolder = config.getStringValue("StaticContent_Folder");
      separatorChar = config.getStringValue("StaticContent_Separator").charAt(0);
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

      String path = staticContentFolder + getPath(request);
      /*
       * If the separator char is not / we want to replace it with a / and
       * canonicalise
       */
      if (File.separatorChar != separatorChar) {
         path = CanonicalPathUtils.canonicalize(
                  path.replace(File.separatorChar, separatorChar));
      }
      if (path.endsWith("/")) {
         try {
            logger.error("Path does not point to specific file");
            response.sendError(StatusCodes.NOT_FOUND);
         } catch (IOException e) {
            logger.error("Error in sending error message to client");
            throw new IOException();
         }
         return;
      }

      // read the file
      FileInputStream inputStream;
      try {
         inputStream = new FileInputStream(path);
      } catch (FileNotFoundException e1) {
         logger.error("File not found");
         throw new FileNotFoundException();
      }

      // Set response header
      response.setHeader("Content-Transfer-Encoding", "UTF-8");

      // Detect file type
      String fileType;
      try {
         fileType = detectFileType(path);
      } catch (Exception e) {
         logger.error("Error in detecting file type");
         throw new ServletException(e.getMessage());
      }

      // Set content type
      response.setContentType(fileType);

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

   /**
    * Detects the file type for setting content type of response header
    * 
    * @param path
    *           Path of file to be serviced
    * @return File type
    * @throws IOException
    * @throws SAXException
    * @throws TikaException
    */
   private String detectFileType(String path)
            throws IOException, SAXException, TikaException {
      File responseFile = new File(path);

      AutoDetectParser parser = new AutoDetectParser();
      parser.setParsers(new HashMap<MediaType, Parser>());

      Metadata metadata = new Metadata();
      metadata.add(TikaMetadataKeys.RESOURCE_NAME_KEY, responseFile.getName());

      InputStream stream = null;
      try {
         stream = new FileInputStream(responseFile);
      } catch (FileNotFoundException e) {
         logger.error("File not found");
         throw new FileNotFoundException();
      }
      try {
         parser.parse(stream, new DefaultHandler(), metadata,
                  new ParseContext());
      } catch (IOException e) {
         logger.error("Error in reading from file");
         throw new IOException();
      } catch (SAXException e) {
         logger.error("Error in detecting file type");
         throw new SAXException();
      } catch (TikaException e) {
         logger.error("Error in detecting file type");
         throw new TikaException(e.getMessage());
      }

      if (stream != null) {
         try {
            stream.close();
         } catch (IOException e) {
            logger.error(
                     "Error in closing input stream (used for detecting file"
                              + " type)");
            throw new IOException();
         }
      }
      String mimeType = metadata.get(HttpHeaders.CONTENT_TYPE);
      return mimeType;
   }

   /**
    * Returns the canonicalized URL path.
    *
    * @param request
    *           Request from client
    * @return canonicalized URL
    */
   private String getPath(final HttpServletRequest request) {
      String servletPath;
      String pathInfo;

      if (request.getDispatcherType() == DispatcherType.INCLUDE && request
               .getAttribute(RequestDispatcher.INCLUDE_REQUEST_URI) != null) {
         pathInfo = (String) request
                  .getAttribute(RequestDispatcher.INCLUDE_PATH_INFO);
         servletPath = (String) request
                  .getAttribute(RequestDispatcher.INCLUDE_SERVLET_PATH);
      } else {
         pathInfo = request.getPathInfo();
         servletPath = request.getServletPath();
      }
      String result = pathInfo;
      if (result == null) {
         result = servletPath;
      } else {
         result = CanonicalPathUtils.canonicalize(result);
      }
      if ((result == null) || (result.equals(""))) {
         result = "/";
      }
      return result;
   }
}