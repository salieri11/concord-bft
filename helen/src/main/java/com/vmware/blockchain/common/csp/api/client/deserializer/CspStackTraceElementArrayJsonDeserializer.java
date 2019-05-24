/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common.csp.api.client.deserializer;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;

/**
 * A deserializer that attempts to convert an array of stacktrace lines to a list of {@link StackTraceElement}s.
 * This is  specific to Csp as csp returns a <code>stackTrace</code> in the ErrorResponse for api calls.
 * This element is an array of Strings, with each element represent a line in a StackTrace.
 * Here's how a sample <code>stackTrace</code> looks like.
 * <pre>
 "stackTrace": [
 "com.vmware.csp.common.utilities.OperationUtil.failOperation(OperationUtil.java:349)",
 "com.vmware.csp.am.account.AccountService.handleGet(AccountService.java:69)",
 "com.vmware.xenon.common.StatefulService.handleRequest(StatefulService.java:334)",
 "com.vmware.xenon.common.StatefulService.lambda$loadAndLinkState$13(StatefulService.java:1009)",
 "com.vmware.xenon.common.Operation.lambda$nestCompletion$0(Operation.java:944)",
 ]
 * </pre>
 * This would return a list of 5 {@link StackTraceElement}s , each with the className, method, file and
 * lineNumber populated.
 * Parts of it is copied from {@link com.fasterxml.jackson.databind.deser.std.StackTraceElementDeserializer}.

 */
public class CspStackTraceElementArrayJsonDeserializer extends JsonDeserializer<List<StackTraceElement>> {

    private static final Pattern STACK_TRACE_PATTERN = Pattern.compile("(.*)\\.(.*)\\(([^:]*):?([\\d]*)\\)");

    private static final int MAX_DEPTH = 10;

    @Override
    public List<StackTraceElement> deserialize(JsonParser p, DeserializationContext ctxt)
            throws IOException, JsonProcessingException {
        List<StackTraceElement> stackTraceElements = null;
        JsonToken t = p.getCurrentToken();
        int curDepth = 1;
        if (t == JsonToken.START_ARRAY) {
            stackTraceElements = new ArrayList<>();
            p.nextToken();
            stackTraceElements.add(getOrNull(p.getValueAsString()));
            JsonToken token = p.nextToken();
            while (token != JsonToken.END_ARRAY && curDepth < MAX_DEPTH) {
                stackTraceElements.add(getOrNull(p.getValueAsString()));
                token = p.nextToken();
                curDepth++;
            }
        }
        return stackTraceElements;
    }

    private StackTraceElement getOrNull(String stackTrace) {
        if (StringUtils.isEmpty(stackTrace)) {
            return null;
        }
        Matcher matcher = STACK_TRACE_PATTERN.matcher(stackTrace);
        if (!matcher.matches()) {
            return null;
        }
        if (matcher.groupCount() < 2) {
            return null;
        }
        try {
            String className = matcher.group(1);
            String methodName = matcher.group(2);
            String fileName = matcher.groupCount() > 3 ? matcher.group(3) : null;
            int lineNum = matcher.groupCount() > 4 ? Integer.parseInt(matcher.group(4)) : -1;
            return new StackTraceElement(className, methodName, fileName, lineNum);
        } catch (Exception e) {
            return null;
        }
    }
}
