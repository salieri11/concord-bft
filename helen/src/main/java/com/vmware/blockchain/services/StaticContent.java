package com.vmware.blockchain.services;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.boot.web.servlet.error.ErrorController;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
public class StaticContent {

   private static final long serialVersionUID = 1L;
   private static final Logger logger
      = LogManager.getLogger(StaticContent.class);

   //TODO: move these strings to properties file
   @RequestMapping(path = "/api")
   public String handleAPIRequest() {
      return "swagger.json";
   }

}
