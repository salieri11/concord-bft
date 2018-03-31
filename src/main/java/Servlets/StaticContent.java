/**
 * This servlet is used to serve static content.
 *
 * Endpoints serviced :
 * /assets/* : Loads content in the priv/www/assets folder
 * /api and /api/ : Loads content from priv/swagger.json
 * /swagger/* : Loads content from priv/www/swagger folder
 * /* : Loads content from priv/www/index.html
 *
 */
package Servlets;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.nio.file.Paths;
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
import org.json.simple.parser.ParseException;
import io.undertow.util.CanonicalPathUtils;
import io.undertow.util.StatusCodes;

/**
 * Servlet class.
 */
public class StaticContent extends HttpServlet {
   private static final long serialVersionUID = 1L;
   private static String staticContentFolder;
   private static char separatorChar;
   private static final Logger logger =
         Logger.getLogger(StaticContent.class);
   private IConfiguration _conf;

   public StaticContent() throws IOException {
      IConfiguration config = FileConfiguration.getInstance();
      staticContentFolder = config.getStringValue("StaticContent_Folder");
      separatorChar = config.getStringValue("StaticContent_Separator")
            .charAt(0);
   }

   /**
    * APIs serviced
    */
   private enum Api {
      ASSETS, SWAGGER, API_LIST, DEFAULT_CONTENT;
   }

   /**
    * Services a get request. Fetches the resource from the specified
    * path and returns it.
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

      String callingApi = request.getServletPath();
      if (callingApi.length() > 0) {
         callingApi = callingApi.substring(1);
      }

      Api api;
      String contentFolder = new String();
      String contentFile = new String();
      String alternateApiListEndpoint = _conf
               .getStringValue("Alternate_ApiList_Endpoint");

      // Read configurations based on API
      switch (callingApi) {

      case "assets":
         logger.trace("/assets API call");
         api = Api.ASSETS;
         contentFolder = _conf
                  .getStringValue("Assets_Folder");
         break;
      case "swagger":
         logger.trace("/swagger API call");
         api = Api.SWAGGER;
         contentFolder = _conf
                  .getStringValue("Swagger_Folder");
         break;
      case "api":
         logger.trace("/api API call");
         api = Api.API_LIST;
         contentFolder = _conf
                  .getStringValue("ApiList_Folder");
         contentFile = _conf
                  .getStringValue("ApiList_File");
         break;
      case "":
         String uri = request.getRequestURI();

         // Load swagger UI if user enters /api/
         if (uri.startsWith(alternateApiListEndpoint)) {
            logger.trace("/api API call");
            contentFolder = _conf
                     .getStringValue("ApiList_Folder");
            contentFile = _conf
                     .getStringValue("ApiList_File");
            api = Api.API_LIST;
         } else {
            logger.trace("Default Content API call");
            api = Api.DEFAULT_CONTENT;
            contentFolder = _conf
                     .getStringValue("Server_DefaultResponse");
            contentFile = "";
         }
         break;
      default:
         return;
      }

      File file = null;

      if (api == Api.ASSETS || api == Api.SWAGGER) {
         // These API calls contain the file path in the request

         String contentPath = StaticContentHelper.getPath(request);
         String path = contentFolder + contentPath;
         /*
          * If the separator char is not / we want to replace it with a / and
          * canonicalise
          */
         if (File.separatorChar != '/') {
            path = CanonicalPathUtils.canonicalize(
                     path.replace(File.separatorChar, '/'));
         }
         if (path.endsWith("/")) {
            try {
               logger.error("Path does not point to specific file");
               response.sendError(StatusCodes.NOT_FOUND);
            } catch (IOException e) {
               logger.error("Error in sending error message to client");
               throw e;
            }
            return;
         }
         file = new File(path);
      } else if (api == Api.API_LIST || api == Api.DEFAULT_CONTENT) {
         // These API calls have fixed content to be served

         // "." denotes current directory
         Path p = Paths.get(".", contentFolder, contentFile);
         file = p.toFile();
      }

      // read the file
      FileInputStream inputStream;
      try {
         inputStream = new FileInputStream(file);
      } catch (FileNotFoundException e) {
         logger.error("File not found : " + file.getAbsolutePath());
         throw e;
      }

      // Detect file type
      String fileType = StaticContentHelper.detectFileType(file);

      // Set file type and charset
      response.setContentType(fileType + ";charset=UTF-8");

      // Respond to client
      int c;
      try {
         byte[] buf = new byte[1024];
         while ((c = inputStream.read(buf, 0, buf.length)) != -1) {
            response.getOutputStream().write(buf, 0, c);
         }
      } catch (IOException e) {
         logger.error("Error in reading from tcp input stream");
         throw e;
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
      metadata.add(TikaMetadataKeys.RESOURCE_NAME_KEY,
                  responseFile.getName());

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

      if (request.getDispatcherType()
            == DispatcherType.INCLUDE
            &&
          request.getAttribute(RequestDispatcher.INCLUDE_REQUEST_URI)
            != null) {
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
