/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

/**
 * Etherum RPC method names and other constants.
 */
public class Constants {
    // Organization Property Keys.
    // Return a list of Zones this org has access to, e.g. ON_PREM or VMC_AWS
    public static final String ORG_ZONES = "org_zones";
    public static final String ORG_DOCKER_IMAGE_OVERRIDE = "org_docker_image_override";
    public static final String ORG_MAX_CHAINS = "max_chains";

    //API Endpoints
    // Below endpoints are needed because some servlets still use this information
    public static final String TRANSACTION_LIST_ENDPOINT = "/api/concord/transactions";

    public static final String API_PREFIX = "/api";

    public static final String AUTH_HEADER_NAME = "csp-auth-token";
    public static final String OAUTH = "/api/oauth";
    public static final String AUTH_INVITATION = OAUTH + "/invitation-return";
    public static final String AUTH_LOGIN = OAUTH + "/login";
    public static final String AUTH_LOGOUT = OAUTH + "/logout";
    public static final String AUTH_LOGIN_RETURN = "/login-return";
    public static final String API_AUTH_TOKEN = OAUTH + "/token";
    // callback is only used by the server and csp
    public static final String OAUTH_CALLBACK = OAUTH + "/oauth";
    public static final String CSP_API_DEFAULT_CLIENT = "csp-api-default-client";
    public static final String CSP_INVITATION_LINK = "serviceInvitationLink";
    public static final String CSP_ORG_LINK = "orgLink";
    public static final String CSP_TOKEN_CACHE = "CspTokenCache";
    public static final String NEW_USER_PARAM = "user";
    public static final String TOKEN_CACHE = "TokenCache";
    public static final String TOKEN_EXPIRES_AT = "token-expires-at";
    public static final String TOKEN_REFRESH = "refresh-token";
    public static final String TOKEN_ID = "token-id";

    // Invitation properties
    // Extra roles to add to the user being invited.
    public static final String INVITATION_ROLE = "invitation_roles";


    // Static content configurations
    public static final String ETHRPC_LIST =
            "[{\"name\": \"eth_accounts\",\"params\": [],\"returns\": \"array\"},"
            + "{\"name\": \"eth_blockNumber\",\"params\": [],\"returns\": \"string\"},"
            + "{\"name\": \"eth_call\",\"params\": [],\"returns\": \"string\"},"
            + "{\"name\": \"eth_coinbase\",\"params\": [],\"returns\": \"string\"},"
            + "{\"name\": \"eth_gasPrice\",\"params\": [],\"returns\": \"string\"},"
            + "{\"name\": \"eth_getBlockByHash\",\"params\": [],\"returns\": \"string\"},"
            + "{\"name\": \"eth_getBlockByNumber\",\"params\": [],\"returns\": \"string\"},"
            + "{\"name\": \"eth_getCode\",\"params\": [],\"returns\": \"string\"},"
            + "{\"name\": \"eth_getFilterChanges\",\"params\": [],\"returns\": \"array\"},"
            + "{\"name\": \"eth_getLogs\",\"params\": [],\"returns\": \"array\"},"
            + "{\"name\": \"eth_getStorageAt\",\"params\": [],\"returns\": \"string\"},"
            + "{\"name\": \"eth_getTransactionCount\",\"params\": [],\"returns\": \"string\"},"
            + "{\"name\": \"eth_getBalance\",\"params\": [],\"returns\": \"string\"},"
            + "{\"name\": \"eth_getTransactionByHash\",\"params\": [],\"returns\": \"object\"},"
            + "{\"name\": \"eth_getTransactionReceipt\",\"params\": [],\"returns\": \"object\"},"
            + "{\"name\": \"eth_mining\",\"params\": [],\"returns\": \"bool\"},"
            + "{\"name\": \"eth_newFilter\",\"params\": [],\"returns\": \"string\"},"
            + "{\"name\": \"eth_newBlockFilter\",\"params\": [],\"returns\": \"string\"},"
            + "{\"name\": \"eth_sendTransaction\",\"params\": [],\"returns\": \"string\"},"
            + "{\"name\": \"eth_sendRawTransaction\",\"params\": [],\"returns\": \"string\"},"
            + "{\"name\": \"eth_syncing\",\"params\": [],\"returns\": \"string\"},"
            + "{\"name\": \"eth_uninstallFilter\",\"params\": [],\"returns\": \"string\"},"
            + "{\"name\": \"net_version\",\"params\": [],\"returns\": \"string\"},"
            + "{\"name\": \"personal_newAccount\",\"params\": [],\"returns\": \"string\"},"
            + "{\"name\": \"rpc_modules\",\"params\": [],\"returns\": \"object\"},"
            + "{\"name\": \"web3_clientVersion\",\"params\": [],\"returns\": \"string\"},"
            + "{\"name\": \"web3_sha3\",\"params\": [],\"returns\": \"string\"}]";
    public static final String RPC_MODULES =
            "[{\"eth\": \"1.0\",\"net\": \"1.0\",\"personal\": \"1.0\",\"rpc\": \"1.0\",\"web3\": \"1.0\"}]";
    public static final String CLIENT_VERSION = "Helen/v1.1.0/linux/java1.8.0";
    public static final int IS_MINING = 0;
    // Gas price is a quantity, which allows odd-nibble values
    public static final String GAS_PRICE = "0x0";

    //Other Constants.  Maybe think if these should be properties
    public static final int BLOCKLIST_DEFAULTCOUNT = 10;
    public static final int TRANSACTIONLIST_DEFAULTCOUNT = 10;
    public static final String BLOCKLIST_URLPREFIX = "/api/concord/blocks/";
    public static final String BLOCKLIST_NEXTPREFIX = "/api/concord/blocks?latest=";
    public static final String TRANSACTION_URLPREFIX = "/api/concord/transactions/";
    public static final String COINBASE = "0x262c0d7ab5ffd4ede2199f6ea793f819e1abb019";

    //EthRPCs
    public static final String JSONRPC = "2.0";
    public static final String SEND_TRANSACTION_NAME = "eth_sendTransaction";
    public static final String SEND_RAWTRANSACTION_NAME = "eth_sendRawTransaction";
    public static final String GET_TRANSACTIONBYHASH_NAME = "eth_getTransactionByHash";
    public static final String GET_TRANSACTIONRECEIPT_NAME = "eth_getTransactionReceipt";
    public static final String GET_STORAGEAT_NAME = "eth_getStorageAt";
    public static final String WEB3_SHA3_NAME = "web3_sha3";
    public static final String RPC_MODULES_NAME = "rpc_modules";
    public static final String CLIENT_VERSION_NAME = "web3_clientVersion";
    public static final String MINING_NAME = "eth_mining";
    public static final String CALL_NAME = "eth_call";
    public static final String NEWACCOUNT_NAME = "personal_newAccount";
    public static final String NETVERSION_NAME = "net_version";
    public static final String ACCOUNTS_NAME = "eth_accounts";
    //Temporary workaround for eth_accounts.  Comma-delimited list of users from our genesis.json
    public static final String USERS = "0x262c0d7ab5ffd4ede2199f6ea793f819e1abb019,"
                                       + "0x5bb088f57365907b1840e45984cae028a82af934,"
                                       + "0x0000a12b3f3d6c9b0d3f126a83ec2dd3dad15f39";
    public static final String NEWFILTER_NAME = "eth_newFilter";
    public static final String NEWBLOCKFILTER_NAME = "eth_newBlockFilter";
    public static final String NEWPENDINGTRANSACTIONFILTER_NAME = "eth_newPendingTransactionFilter";
    public static final String FILTERCHANGE_NAME = "eth_getFilterChanges";
    public static final String UNINSTALLFILTER_NAME = "eth_uninstallFilter";
    public static final String GET_CODE_NAME = "eth_getCode";
    public static final String COINBASE_NAME = "eth_coinbase";
    public static final String GET_BLOCKBYHASH_NAME = "eth_getBlockByHash";
    public static final String BLOCKNUMBER_NAME = "eth_blockNumber";
    public static final String GET_BLOCKBYNUMBER_NAME = "eth_getBlockByNumber";
    public static final String GET_TRANSACTIONCOUNT_NAME = "eth_getTransactionCount";
    public static final String GET_BALANCE_NAME = "eth_getBalance";
    public static final String GAS_PRICE_NAME = "eth_gasPrice";
    public static final String ESTIMATE_GAS_NAME = "eth_estimateGas";
    public static final String SYNCING_NAME = "eth_syncing";
    public static final String GET_LOGS_NAME = "eth_getLogs";

    public static final String URI_PATH_CHAR = "/";

    public static final String USE_CSP_AUTH = "vmbc.auth.csp";

}
