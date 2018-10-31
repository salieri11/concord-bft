/**
 * Copyright 2018 VMware, all rights reserved.
 *
 */

package Servers;

import java.io.IOException;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.io.Resource;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;
import org.springframework.web.servlet.resource.PathResourceResolver;

@Configuration
public class HelenSpringWebConfig implements WebMvcConfigurer {

    // Value annotation is a new spring feature which allows  you to inject
    // values from properties file directly
    @Value("${STATIC_RESOURCE_LOCATION}")
    private String staticResourcePath;

    @Value("${API_URI_PREFIX}")
    private String apiUriPrefix;

    @Value("${HOME_PAGE_LOCATION}")
    private String homePage;

    @Override
    public void addResourceHandlers(ResourceHandlerRegistry registry) {

        // Add patterns for static resource requests
        // TODO: Maybe move this list into properties file
        registry.addResourceHandler("/**/*.css",
                "/**/*.html",
                "/**/*.js",
                "/**/*.jsx",
                "/**/*.png",
                "/**/*.ttf",
                "/**/*.woff",
                "/**/*.woff2",
                "/**/*.json")
                .addResourceLocations(staticResourcePath);

        // All requests which are not mapped to any controller (i.e which there
        // is not @RequestMapping for that URI) and do not follow above static
        // resource definition will be handled by below handler. The
        // default action of this handler is to simply redirect all such
        // queries to index.html
        registry.addResourceHandler("/", "/**")
                .addResourceLocations(homePage)
                .resourceChain(true)
                .addResolver(new PathResourceResolver() {
                    @Override
                    protected Resource
                    getResource(String resourcePath,
                                Resource location) throws IOException {
                        if (resourcePath.startsWith(apiUriPrefix)
                                || resourcePath.startsWith(apiUriPrefix.substring(1))) {
                            return null;
                        }

                        return location.exists() && location.isReadable() ? location
                                : null;
                    }
                });
    }

}
