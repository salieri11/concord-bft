package Servlets;

import com.vmware.athena.Athena;
import org.apache.log4j.Logger;
import org.json.simple.JSONAware;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;
import profiles.UsersRegistryManager;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class ProfileManager extends BaseServlet {

    private static final long serialVersionUID = 1L;
    private static final Logger logger = Logger.getLogger(ProfileManager.class);
    private UsersRegistryManager urm;
    private Pattern uriPattern = Pattern.compile("(/api/athena/?)" +
            "([a-zA-Z0-9]/?)");
            
    
    private ProfileManager() {
        try {
            urm = UsersRegistryManager.getInstance();
        } catch (Exception e) {
            logger.fatal("Unable to instantiate UsersRegistryManager", e);
            urm = null;
        }
    }
    
    @Override
    protected void service(HttpServletRequest request, HttpServletResponse
            response) throws ServletException, IOException  {
        if (request.getMethod().equalsIgnoreCase("PATCH")) {
            doPatch(request, response);
        } else {
            super.service(request, response);
        }
    }
    
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        System.out.println("URI: " + request.getRequestURI() + " URL: " +
                request.getRequestURL());
        
        String uri = request.getRequestURI();
        if (uri.endsWith("/"))
            uri = uri.substring(0, uri.length() -1 );
        String uriTokens[] = request.getRequestURI().split("/");
        
        String consortium = request.getParameter("consortium");
        String organization = request.getParameter("organization");
        if (consortium != null && organization != null
                && uriTokens.length == 3) {
            // /api/user?consortium=<c>&organization=<o>
            processResponse(response,
                    "{}",
                    HttpServletResponse.SC_OK, logger);
        } else if (uriTokens.length == 4) {
            // /api/user/<userid>
            processResponse(response,
                    "{}",
                    HttpServletResponse.SC_OK, logger);
        } else {
            processResponse(response, "",
                    HttpServletResponse.SC_NOT_FOUND, logger);
        }
    }
    
    
    @Override
    protected void
    doPost(final HttpServletRequest request,
           final HttpServletResponse response) throws IOException {
        String responseString;
        int responseStatus;
        try {
            String paramString
                    = request.getReader()
                    .lines()
                    .collect(Collectors.joining(System.lineSeparator()));
            logger.debug(paramString);
            JSONParser parser = new JSONParser();
            JSONObject requestObject = (JSONObject) parser.parse(paramString);
            
            String name = (String) requestObject.get("name");
            String email = (String) requestObject.get("email");
            String role = (String) requestObject.get("role");
            String consortiumId = (String) requestObject.get("consortium_id");
            String organizationId =
                    (String) requestObject.get("organization_id");
            
            // check if organization ID and consortium ID are valid
            responseString = "{}";
            responseStatus = 0;
        } catch (ParseException pe) {
            logger.warn("Error while parsing request JSON", pe);
            responseString = APIHelper.errorJSON("Invalid JSON").toJSONString();
            responseStatus = HttpServletResponse.SC_BAD_REQUEST;
        }
        processResponse(response, responseString, responseStatus, logger);
    }
    
    
    protected void doPatch(HttpServletRequest request, HttpServletResponse
            response) {
        
    }
    
    
    @Override
    protected JSONAware parseToJSON(Athena.AthenaResponse athenaResponse) {
        return null;
    }
}
