/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.deployment.services.restclient.interceptor;

import java.net.URI;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

import org.springframework.web.util.UriComponentsBuilder;
import org.springframework.web.util.UriTemplate;

/**
 * A few utilites used in the csp package.
 */
public class SanitizeUtils {

    /**
     * Checks and Sanitizes the url and removes any sensitive path params/query params. The uri is checked against the
     * set of URL templates that provided. For example if the following url is provided
     * <code>/this/is/a/sensitive-path-param/url?
     * nonSensitiveParam1=aValue&nonSensitiveParam2=aValue2&sensitiveParam3=aSensitiveValue</code>
     * and the set urisToSanitize contains <code>/this/is/a/{templateParam}/url</code>, the output will be
     * <code>/this/is/a/{templateParam}/url?nonSensitiveParam1=aValue&nonSensitiveParam2=aValue2&sensitiveParam3=</code>
     * @param uri - The uri to check if it needs to be sanitized
     * @param urisToSanitize - The set of uris that require cleaning.
     * @param queryParamsToSanitize - The set of query params that require cleaning.
     */
    public static String cleanUri(URI uri, Set<UriTemplate> urisToSanitize, Set<String> queryParamsToSanitize) {
        Objects.requireNonNull(uri);
        Objects.requireNonNull(urisToSanitize);
        Objects.requireNonNull(queryParamsToSanitize);

        Optional<UriTemplate> first =
                urisToSanitize.stream().filter(noOpUrl -> noOpUrl.matches(uri.getPath())).findFirst();
        //Replace the path with template if the URL has sensitive components, else log the actual URL.
        String urlToLog =
                first.isPresent() ? UriComponentsBuilder.fromUri(uri).replacePath(first.get().toString()).build()
                        .toString()
                                  : uri.toString();
        //Clean any request parameters.
        UriComponentsBuilder clean = UriComponentsBuilder.fromUriString(urlToLog);
        //set the value to null.
        queryParamsToSanitize.stream().forEach(qp -> clean.replaceQueryParam(qp));
        return clean.build().toString();
    }

}
