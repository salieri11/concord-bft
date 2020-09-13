package com.vmware.blockchain.api.test.blockchains;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.logging.Logger;

import org.junit.Test;

import com.vmware.blockchain.api.test.base.ApiTestBase;
import com.vmware.blockchain.api.test.config.ApiTestConfig;
import com.vmware.blockchain.api.test.util.ConfigUtil;

import io.swagger.client.ApiException;
import io.swagger.client.api.BlockchainsApi;
import io.swagger.client.model.BaseZoneGet;
import io.swagger.client.model.Body;
import io.swagger.client.model.ClientNode;
import io.swagger.client.model.InlineResponse200;
import io.swagger.client.model.InlineResponse2001;
import io.swagger.client.model.InlineResponse20012;
import io.swagger.client.model.InlineResponse2003;
import io.swagger.client.model.InlineResponse2004;
import io.swagger.client.model.InlineResponse202;
import io.swagger.client.model.Zone;
import io.swagger.client.model.InlineResponse20013;
import io.swagger.client.model.InlineResponse2005;


/**
 * API tests for BlockchainsApi
 */
public class BlockchainApiTest extends ApiTestBase {

    private final BlockchainsApi api = new BlockchainsApi();
    private static UUID bId;
    private static UUID zoneId;
    private static UUID replicaId;

    private static final Logger logger = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

    /**
     * Create a new blockchain
     *
     *
     *
     * @throws ApiException
     *          if the Api call fails
     */
    @Test
    public void blockchainPostTest() throws ApiException {
        // Get the config object to read the custom/default values.
        ApiTestConfig apiTestConfig = ConfigUtil.getApiTestConfig();

        api.getApiClient().setBasePath(apiTestConfig.getApiServiceUrl());
        api.getApiClient().addDefaultHeader(apiTestConfig.HTTP_ACCEPT_PARAM, apiTestConfig.HTTP_ACCEPT_TYPE_JSON);

        String accessToken = authMap.get(apiTestConfig.ACCESS_TOKEN_PARAM);
        api.getApiClient().addDefaultHeader(apiTestConfig.getCspAuthHeader(), accessToken);
        logger.info("API base path: " + api.getApiClient().getBasePath());
        logger.info("API user authentication: " + api.getApiClient().getAuthentications());

        Body body = new Body();
        body.setBlockchainType(Body.BlockchainTypeEnum.DAML);
        List<ClientNode> clientNodes = new ArrayList<>();
        ClientNode cNode = new ClientNode();
        cNode.setZoneId(UUID.randomUUID());
        cNode.setAuthUrlJwt(apiTestConfig.TOKEN_PARAM);
        body.setClientNodes(clientNodes);
        body.setConsortiumId(UUID.randomUUID());
        List<UUID> replicaZones = new ArrayList<>();
        //TODO: Add some zones.
        //body.setReplicaZoneIds(replicaZones);
        logger.info("sending request........." + api);
        InlineResponse202 response1 = api.blockchainPost(body);
        logger.info("response = " + response1);

        logger.info("task id " + response1.getTaskId());
    }

    @Test
    public void getBlockchains() throws ApiException {
        List<InlineResponse200> list = api.getBlockchains();
        logger.info("list = " + list);
        bId = (list != null && !list.isEmpty()) ? list.get(0).getId() : null;
        logger.info("bId now " + bId);
    }

    @Test
    public void getABlockchain() throws ApiException {
        if (bId != null) {
            InlineResponse2001 response = api.getBlockchain(bId);
            logger.info("Response  = " + response);
        }
    }

    @Test
    public void getZones() throws ApiException {
        if (bId != null) {
            List<BaseZoneGet> zoneResponse = api.getBlockchainZones();
            logger.info("Zone Response  = " + zoneResponse);
        }
    }


    @Test
    public void getReplicas() throws ApiException {
        if (bId != null) {
            List<InlineResponse2004> response = api.listReplicas(bId);
            logger.info("Response  = " + response);
            if (response != null && !response.isEmpty()) {
                replicaId = response.get(0).getId();
                zoneId = response.get(0).getZoneId();
            }
        }
    }

    @Test
    public void getClients() throws ApiException {
        if (bId != null) {
            List<InlineResponse20013> response = api.getClients(bId);
            logger.info("Response  = " + response);
            if (response != null && !response.isEmpty()) {
                zoneId = response.get(0).getZoneId();
            }
        }
    }



    @Test
    public void getOnpremZones() throws ApiException {
        if (zoneId != null) {
            Zone zone = api.getOnpremZones(zoneId);
            logger.info("Response  = " + zone);
        }
    }

    @Test
    public void getReplicaCredentials() throws ApiException {
        if (bId != null && replicaId != null) {
            InlineResponse2005 response = api.getReplicaCredentials(bId, replicaId);
            logger.info("Response  = " + response);
        }
    }
}

