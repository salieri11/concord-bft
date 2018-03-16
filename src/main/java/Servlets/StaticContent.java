/**
 * url endpoint : /static/*
 * 
 * This servlet is used to serve static content.
 */
package Servlets;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

import javax.servlet.DispatcherType;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import io.undertow.util.CanonicalPathUtils;
import io.undertow.util.StatusCodes;

/**
 * Servlet class.
 */
public class StaticContent extends HttpServlet {

    private static final long serialVersionUID = 1L;

    /**
     * Services a get request. Fetches the resource from the specified path and
     * returns it.
     * 
     * @param request
     *            The request received by the servlet
     * @param response
     *            The response object used to respond to the client
     */
    protected void doGet(final HttpServletRequest request,
            final HttpServletResponse response)
            throws ServletException, IOException {

        String path = "static" + getPath(request);

        /* If the separator char is not / we want to replace it with a / and
         * canonicalise*/
        if (File.separatorChar != '/') {
            path = CanonicalPathUtils
                    .canonicalize(path.replace(File.separatorChar, '/'));
        }
        if (path.endsWith("/")) {
            response.sendError(StatusCodes.NOT_FOUND);
            return;
        }

        // read the file
        FileInputStream inputStream = new FileInputStream(path);

        //Set response headers
        response.setHeader("Content-Transfer-Encoding", "UTF-8");
        response.setContentType("image/jpg");
        response.setContentType("text/html");
        
        //Respond to client
        try {
            int c;
            while ((c = inputStream.read()) != -1) {
                response.getWriter().write(c);
            }
        } finally {
            //close resources
            if (inputStream != null)
                inputStream.close();
            response.getWriter().close();
        }
    }

    /**
     * Returns the canonicalized URL path.
     *
     * @param request Request from client
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