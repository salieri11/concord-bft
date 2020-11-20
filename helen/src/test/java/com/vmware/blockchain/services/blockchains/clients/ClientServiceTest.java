/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.services.blockchains.clients;

import static com.vmware.blockchain.services.blockchains.clients.ClientController.ClientPatch;
import static org.junit.Assert.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.UUID;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import com.vmware.blockchain.common.HelenException;
import com.vmware.blockchain.dao.GenericDao;

/**
 * ClientService Tests.
 * */
@ExtendWith(SpringExtension.class)
public class ClientServiceTest {

    static final UUID BC_DAML = UUID.fromString("fd7167b0-057d-11ea-8d71-362b9e155667");
    private static final UUID SITE_1 = UUID.fromString("84b9a0ed-c162-446a-b8c0-2e45755f3844");
    private static final UUID CLIENT_NODE_ID = UUID.fromString("7eef6110-68bc-11ea-906e-8c859085f3e7");

    private ClientService clientService;

    @MockBean
    private GenericDao genericDao;

    @BeforeEach
    void init() {
        clientService = new ClientService(genericDao);
    }

    /**
     * Tests happy path.
     * */
    @Test
    public void testUpdate() {
        final String newPass = "my_pass";
        final Client client1 = new Client("publicIp", "privateIp", "hostName", "url",
                                          "cert", "pass", BC_DAML, SITE_1, CLIENT_NODE_ID, null, "pem", "crt", "cacrt");

        Client updated = new Client();
        updated.setDamlDbPassword(newPass);
        when(genericDao.put(any(Client.class), any(Client.class))).thenReturn(updated);

        ClientPatch cp = new ClientPatch();
        cp.setDamlDbPassword(newPass);

        Client client = clientService.updateClient(client1, cp);
        assertEquals(newPass, client.getDamlDbPassword());
    }

    /**
     * Tests error triggered by empty password.
     * */
    @Test
    public void testUpdateEmptyPwd() {
        final String newPass = "my_pass";
        final Client client1 = new Client("publicIp", "privateIp", "hostName", "url",
                                          "cert", "pass", BC_DAML, SITE_1, CLIENT_NODE_ID, null, "pem", "crt", "cacrt");

        ClientPatch cp2 = new ClientPatch();
        cp2.setDamlDbPassword("");

        HelenException exc = assertThrows(HelenException.class, () -> clientService.updateClient(client1, cp2));
        assertEquals(exc.getHttpStatus(), HttpStatus.BAD_REQUEST);
    }

}
