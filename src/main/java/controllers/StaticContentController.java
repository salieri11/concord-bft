package controllers;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.boot.web.servlet.error.ErrorController;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
public class StaticContentController implements ErrorController {

    private static final String errorPath = "/error";

    private static final long serialVersionUID = 1L;
    private static final Logger logger =
            LogManager.getLogger(StaticContentController.class);

    
    @RequestMapping(path = "/api")
    public String handleAPIRequest() {
        return "swagger.json";
    }
    
    public String handleErroneousRequest() {
        // Redirect all 404 requests to homepage
        return "index.html";
    }
    
    @Override
    public String getErrorPath() {
        return errorPath;
    }
    
}
