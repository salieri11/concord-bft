/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.lint;

import java.sql.SQLSyntaxErrorException;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.test.context.junit.jupiter.SpringExtension;

/**
 * Test the SimpleSqlParser.
 */
@ExtendWith(SpringExtension.class)
public class SqlParserTests {

    @Test
    void testBasic() throws Exception {
        SimpleSqlParser p = new SimpleSqlParser("select * from table");
        Assertions.assertEquals("SELECT * FROM table", p.toSql());
    }

    @Test
    void testHaving() throws Exception {
        SimpleSqlParser p = new SimpleSqlParser("select * from table having a or b");
        Assertions.assertEquals("SELECT * FROM table having a or b", p.toSql());
    }

    @Test
    void testMoreComplex() throws Exception {
        SimpleSqlParser p = new SimpleSqlParser("select * from table group by a having a or b");
        Assertions.assertEquals("SELECT * FROM table group by a having a or b", p.toSql());
    }

    @Test
    void testWhere() throws Exception {
        SimpleSqlParser p = new SimpleSqlParser("select * from table where username = 'user'");
        Assertions.assertEquals("SELECT * FROM table where username = 'user'", p.toSql());
    }

    @Test
    void testSimpleWhere() throws Exception {
        SimpleSqlParser p = new SimpleSqlParser("select * from table");
        p.addWhere("username = 'user'");
        Assertions.assertEquals("SELECT * FROM table WHERE username = 'user'", p.toSql());
    }

    @Test
    void testBadSql() throws Exception {
        Assertions.assertThrows(SQLSyntaxErrorException.class, () -> new SimpleSqlParser("Argle Bargle"));
    }

    @Test
    void testMoreComplexWhere() throws Exception {
        SimpleSqlParser p = new SimpleSqlParser("select * from table group by a having a or b");
        p.addWhere("username = 'user'");
        Assertions.assertEquals("SELECT * FROM table WHERE username = 'user' group by a having a or b", p.toSql());
    }

    @Test
    void testWhereAdd() throws Exception {
        SimpleSqlParser p = new SimpleSqlParser("select * from table where username = 'user'");
        p.addWhere("consortium_id = 'one' OR org_id = 'two'");
        Assertions.assertEquals(
                "SELECT * FROM table where (consortium_id = 'one' OR org_id = 'two') AND (username = 'user')",
                p.toSql());
    }

    @Test
    void testMoreComplexWhereAdd() throws Exception {
        SimpleSqlParser p = new SimpleSqlParser("select * from table where username = 'user' group by a having a or b");
        p.addWhere("consortium_id = 'one' OR org_id = 'two'");
        Assertions.assertEquals(
                "SELECT * FROM table where (consortium_id = 'one' OR org_id = 'two') "
                + "AND (username = 'user' ) group by a having a or b",
                p.toSql());
    }

}
