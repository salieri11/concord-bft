/**
 * Copyright (c) 2018 VMware, Inc. All rights reserved. VMware Confidential
 */

package configurations;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import lombok.Getter;
import lombok.Setter;

@Component
@Getter
@Setter
public class AthenaProperties {

    //Athena configurations
    @Value("${AthenaAuthorities}") String AthenaAuthorities;
    @Value("${ConnectionPoolSize}") int ConnectionPoolSize;
    @Value("${ConnectionPoolFactor}") int ConnectionPoolFactor;
    @Value("${ConnectionPoolWaitTimeoutMs}") int ConnectionPoolWaitTimeoutMs;
    @Value("${ReceiveTimeoutMs}") int ReceiveTimeoutMs;
    @Value("${ReceiveHeaderSizeBytes}") int ReceiveHeaderSizeBytes;

    //API Endpoints
    // Below endpoints are needed because some servlets still use this information
    @Value("${TransactionList_Endpoint}") String TransactionList_Endpoint;
    @Value("${Contracts_Endpoint}") String Contracts_Endpoint;
    @Value("${API_URI_PREFIX}") String API_URI_PREFIX;
    @Value("${STATIC_RESOURCE_LOCATION}") String STATIC_RESOURCE_LOCATION;
    @Value("${HOME_PAGE_LOCATION}") String HOME_PAGE_LOCATION;


    //Static content configurations
    @Value("${EthRPCList}") String EthRPCList;
    @Value("${RPCModules}") String RPCModules;
    @Value("${ClientVersion}") String ClientVersion;
    @Value("${Is_Mining}") int Is_Mining;
    // Gas price is a quantity, which allows odd-nibble values
    @Value("${GasPrice}") int GasPrice;

    //Other Constants
    @Value("${BlockList_DefaultCount}") long BlockList_DefaultCount;
    @Value("${TransactionList_DefaultCount}") long TransactionList_DefaultCount;
    @Value("${BlockList_URLPrefix}") String BlockList_URLPrefix;
    @Value("${BlockList_NextPrefix}") String BlockList_NextPrefix;
    @Value("${Transaction_URLPrefix}") String Transaction_URLPrefix;
    @Value("${Coinbase}") String Coinbase;

    //EthRPCs
    @Value("${JSONRPC}") String JSONRPC;
    @Value("${SendTransaction_Name}") String SendTransaction_Name;
    @Value("${SendRawTransaction_Name}") String SendRawTransaction_Name;
    @Value("${GetTransactionReceipt_Name}") String GetTransactionReceipt_Name;
    @Value("${GetStorageAt_Name}") String GetStorageAt_Name;
    @Value("${Web3SHA3_Name}") String Web3SHA3_Name;
    @Value("${RPCModules_Name}") String RPCModules_Name;
    @Value("${ClientVersion_Name}") String ClientVersion_Name;
    @Value("${Mining_Name}") String Mining_Name;
    @Value("${Call_Name}") String Call_Name;
    @Value("${NewAccount_Name}") String NewAccount_Name;
    @Value("${NetVersion_Name}") String NetVersion_Name;
    @Value("${Accounts_Name}") String Accounts_Name;
    //Temporary workaround for eth_accounts.  Comma-delimited list of users.
    @Value("${USERS}") String USERS;
    @Value("${NewFilter_Name}") String NewFilter_Name;
    @Value("${NewBlockFilter_Name}") String NewBlockFilter_Name;
    @Value("${NewPendingTransactionFilter_Name}") String NewPendingTransactionFilter_Name;
    @Value("${FilterChange_Name}") String FilterChange_Name;
    @Value("${UninstallFilter_Name}") String UninstallFilter_Name;
    @Value("${GetCode_Name}") String GetCode_Name;
    @Value("${Coinbase_Name}") String Coinbase_Name;
    @Value("${GetBlockByHash_Name}") String GetBlockByHash_Name;
    @Value("${BlockNumber_Name}") String BlockNumber_Name;
    @Value("${GetBlockByNumber_Name}") String GetBlockByNumber_Name;
    @Value("${GetTransactionCount_Name}") String GetTransactionCount_Name;
    @Value("${GetBalance_Name}") String GetBalance_Name;
    @Value("${GasPrice_Name}") String GasPrice_Name;
    @Value("${Syncing_Name}") String Syncing_Name;

    public AthenaProperties instance() {
        AthenaProperties prop = new AthenaProperties();
        BeanUtils.copyProperties(this, prop);
        return prop;
    }
}
