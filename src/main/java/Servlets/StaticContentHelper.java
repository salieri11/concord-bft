/**
 * Contains helper functions for serving static content.
 */
package Servlets;

import java.io.File;
import java.io.IOException;
import javax.servlet.DispatcherType;
import javax.servlet.RequestDispatcher;
import javax.servlet.http.HttpServletRequest;
import org.apache.tika.Tika;
import io.undertow.util.CanonicalPathUtils;

public class StaticContentHelper {

   /**
    * Detects the file type for setting content type of response header. Note :
    * The file extension is used for deducing the type.
    *
    * @param file
    *           File to be served
    * @return File type
    * @throws IOException
    */
   public static String detectFileType(File file) throws IOException {
      Tika tika = new Tika();
      return tika.detect(file.getName());
   }

   /**
    * Returns the canonicalized URL path.
    *
    * @param request
    *           Request from client
    * @return canonicalized URL
    */
   public static String getPath(final HttpServletRequest request) {
      String servletPath;
      String pathInfo;

      if (request.getDispatcherType() == DispatcherType.INCLUDE
         && request.getAttribute(RequestDispatcher.INCLUDE_REQUEST_URI) != null) {
         pathInfo
            = (String) request.getAttribute(RequestDispatcher.INCLUDE_PATH_INFO);
         servletPath
            = (String) request.getAttribute(RequestDispatcher.INCLUDE_SERVLET_PATH);
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
