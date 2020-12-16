/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.lint;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.vmware.blockchain.common.BadRequestException;
import com.vmware.blockchain.common.ErrorCodeType;

/**
 * A very, very simple sql parser using regular expressions.
 * It will attempt to parse queries of the from "SELECT columns FROM tables [WHERE condition] [rest of query]
 * The where clause is optional.  the rest of the query is determined by everything that comes after one of the
 * following keywords: GROUP BY, HAVING, WINDOW, ORDER BY, LIMIT, INTO, FOR.
 *
 *  <p>This will not handle nested queries.  If we need something more complicated, we'll need to pull in a real parser.
 */
public class SimpleSqlParser {
    private String select;
    private String from;
    private String whereKey;
    private String where;
    private String keyword;
    private String restOfQuery;

    String queryPattern = "select (.+)"
            + "from (.+?)"
            + "(?:( ?where )(.+?))?"
            + "(?:(group by|having|window|order by|limit|into|for) (.+?))?";

    Pattern sqlPattern = Pattern.compile(queryPattern, Pattern.CASE_INSENSITIVE);

    /**
     * Try to parse the query.  Throw an exception if this doesn't match.
     */
    public SimpleSqlParser(String query) {
        Matcher m = sqlPattern.matcher(query);
        if (!m.matches()) {
            throw new BadRequestException(ErrorCodeType.UNMATCHED_QUERY, query);
        }
        // The groups in the match are 1) select 2) from 3) where (might be null) 4) keyword 5) rest of query
        select = m.group(1);
        from = m.group(2);
        whereKey = m.group(3);
        where = m.group(4);
        keyword = m.group(5);
        restOfQuery = m.group(6);
    }


    /**
     * If the where clause exists, set it to "(condition) AND (existingCondition)",
     * otherwise just set it.
     */
    public void addWhere(String condition) {
        if (where == null) {
            // need to fix spaces depending on whether or not the query ended with the FROM,
            // or another keyword
            if (keyword == null) {
                whereKey = " WHERE ";
                where = condition;
            } else {
                whereKey = "WHERE ";
                where = condition + " ";
            }
        } else {
            where = String.format("(%s) AND (%s)%s", condition, where, keyword == null ? "" : " ");
        }
    }

    /**
     * Convert this to a sql statement.
     */
    public String toSql() {
        StringBuilder builder = new StringBuilder("SELECT ").append(select)
                .append("FROM ").append(from);
        if (where != null) {
            builder = builder.append(whereKey).append(where);
        }

        if (keyword != null) {
            builder = builder.append(keyword).append(" ").append(restOfQuery);
        }
        return builder.toString();
    }

}
