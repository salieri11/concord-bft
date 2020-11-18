/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.server;

import java.security.Security;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import com.vmware.blockchain.configuration.eccerts.ConcordEcCertificatesGenerator;
import com.vmware.blockchain.deployment.v1.ConcordComponent;
import com.vmware.blockchain.deployment.v1.ConfigurationComponent;
import com.vmware.blockchain.deployment.v1.IdentityComponent;
import com.vmware.blockchain.server.util.IdentityComponentsLists;

/**
 * Test for ConfigurationServiceHelper.
 */
public class ConfigurationServiceHelperTest {

    private static final String certData =
            "-----BEGIN CERTIFICATE-----\nMIIDBTCCAoygAwIBAgIUA1X5ubDUvXdhuA3MrCStTvTSCdQwCgYIKoZIzj0EAwIw\nUzELMAkGA1"
            + "UEBhMCTkExCzAJBgNVBAgMAk5BMQswCQYDVQQHDAJOQTELMAkGA1UE\nCgwCTkExCjAIBgNVBAsMATAxETAPBgNVBAMMCG"
            + "5vZGUwc2VyMB4XDTIwMTExMjIz\nNDM0OFoXDTIzMTExMjIzNDM0OFowUzELMAkGA1UEBhMCTkExCzAJBgNVBAgMAk5B\n"
            + "MQswCQYDVQQHDAJOQTELMAkGA1UECgwCTkExCjAIBgNVBAsMATAxETAPBgNVBAMM\nCG5vZGUwc2VyMHYwEAYHKoZIzj0CAQYFK4EEA"
            + "CIDYgAE/BkXQ6kNs95Ip/mRYvnn\nuxehPAOc0mCgl4bamz10a+QDLYXkvn4rPc4Q/V5Okney/UxqEfNsnwZPFLtuKGAB\nwJ"
            + "RWnKEkS33AbZVcT1vIzZcpMivmuUuGVpcuQ0A7ldDvo4IBHzCCARswgYEGA1Ud\nDgR6BHgwdjAQBgcqhkjOPQIBBgUrgQQAIgNiA"
            + "AT8GRdDqQ2z3kin+ZFi+ee7F6E8\nA5zSYKCXhtqbPXRr5AMtheS+fis9zhD9Xk6Sd7L9TGoR82yfBk8Uu24oYAHAlFac\noSRLfcB"
            + "tlVxPW8jNlykyK+a5S4ZWly5DQDuV0O8wgYMGA1UdIwR8MHqAeDB2MBAG\nByqGSM49AgEGBSuBBAAiA2IABPwZF0OpDbPeSKf5kWL"
            + "557sXoTwDnNJgoJeG2ps9\ndGvkAy2F5L5+Kz3OEP1eTpJ3sv1MahHzbJ8GTxS7bihgAcCUVpyhJEt9wG2VXE9b\nyM2XKTIr"
            + "5rlLhlaXLkNAO5XQ7zAPBgNVHRMBAf8EBTADAQH/MAoGCCqGSM49BAMC\nA2cAMGQCMGalMTNSmTn6rzGRFrKEuXE2z5r1lmTbNFun"
            + "tLrRu06EYdGCgrHgtPz3\nikhA/0bbcQIwTO46ao9WFWDi6gGkXXINqeQBPgYrDCodfK1GEoZvMRROeS1qFO/t\nY5Ed+TBtPdBr\n"
            + "-----END CERTIFICATE-----\n";
    private static final String keyData = "-----BEGIN EC PRIVATE KEY-----\nMIGkAgEBBDDlsBpngltoneWvduhx5xgE2H6T9GgxG8e"
                                          + "kzEnslkVLaKwQ2GoKPLLz\neDBUJLIAL52gBwYFK4EEACKhZANiAATZUzZwwRrMnW6EQKoAZ"
                                          + "4E9BSpaAM/YY9mh\nHFfRTHV66LBRVj2I0yeJbsYVEZIeMtEatemGA63pcGrGu0irReTkxV3G6"
                                          + "u5Iju1g\nwcqYkqgIEEUJb3XqTCHrSP2jBF+AtJ8=\n-----END EC PRIVATE KEY-----"
                                          + "\n";
    static String telegrafComponentValue = "# Global tags can be specified here in key=\"value\" format."
                                           + "\n[global_tags]\n  "
                                           + "blockchain = \"f84d40ce-e06b-4c93-93d5-8db791cd7d30\"\n  "
                                           + "consortium = "
                                           + "\"9a1d42db-0fe9-4208-82a5-58368b59a2a9\"\n  vm_ip = \"192.168.0.4\"\n  "
                                           + "vm_type = \"replica\"\n  source = \"f84d40ce-e06b-4c93-93d5-"
                                           + "8db791cd7d30\"\n\n# "
                                           + "Configuration for telegraf agent\n[agent]\n  interval = \"10s\"\n  "
                                           + "round_interval = true\n\n  metric_batch_size = 1000\n\n  metric_"
                                           + "buffer_limit = "
                                           + "10000\n\n  collection_jitter = \"0s\"\n\n  flush_interval = \"10s\"\n  "
                                           + "\"http://concord:9891/metrics\",\"http://daml_execution_engine:55001/"
                                           + "metrics\""
                                           + "]\n\n# Send epoch time for each node\n[[inputs.exec]]\n  commands = "
                                           + "[\"date "
                                           + "+%s\",]\n  timeout=\'10s\'\n  name_override=\"time.epoch\"\n  "
                                           + "data_format="
                                           + "\"value\"\n  data_type=\"long\"";


    static String secretConfig = "commit_public_key: 0317e69cd44dc1d4446ad51d2512ffeb65d632a2d2b0b3288f19d61483972cbff9"
                                 + "0b6a7b7934f037c92aff12ad9203ee202743a02f4768aae58d75335dd4c4fcab\n"
        + "commit_cryptosys: threshold-bls BN-P254\n"
        + "optimistic_commit_cryptosys: multisig-bls BN-P254\n"
        + "optimistic_commit_public_key: 03076ef8a5846b1262126bfbee8756aa0f99fe7165b67cab53ec7b13aab6d7f1e414bdd"
                                     + "b996d9e90ad59c6bc2c94378463ecf05dbe861d2282402e08a00a1bb525\n"
        + "slow_commit_cryptosys: threshold-bls BN-P254\n"
        + "slow_commit_public_key: 020d6d75c1e39271ece5aa81f8d6a7274ef1f8ff1fd022c68fac909a2020ed5d0312db848"
                                     + "26e50b19cd10be5f57a5580a81a1a4a637c2bea1160fb2b9fee95c22e\n"
        + "tls_cipher_suite_list: ECDHE-ECDSA-AES256-GCM-SHA384\n"
        + "node:\n"
        + "       - client_proxy:\n"
        + "       - principal_id: 4\n"
        + "       - principal_id: 8\n"
        + "       - principal_id: 12\n"
        + "       - principal_id: 16\n"
        + "replica:\n"
        + "        - commit_verification_key: 030a9741a71348cd0c5d349d85a30259fc18982038336be0eea69289ee2f385a5a00b"
                + "9f6bb7eaa2ef5274700c1f6ed8ebf351adcc061c69b62317c71a33d2cf13c\n"
        + "optimistic_commit_verification_key: 0212cb7f";

    static String deployConfig = "create_tee_genesis_block: true\n"
        + "c_val: 0\n"
        + "client_proxies_per_replica: 4\n"
        + "daml_enable: true\n"
        + "eth_enable: false\n"
        + "f_val: 1\n"
        + "insecure_thin_replica_server: true\n"
        + "num_client_proxies: 16\n"
        + "num_of_external_clients: 2\n"
        + "num_principals: 20\n"
        + "num_replicas: 4\n"
        + "num_ro_replicas: 0\n"
        + "perf_enable: false\n"
        + "preexecution_enabled: true\n"
        + "tee_enable: false\n"
        + "node:\n"
        + "       - enable_histograms_or_summaries: true\n"
        + "time_source_id: time-source0\n"
        + "client_proxy:\n"
        + "        - client_host: 192.168.0.1\n"
        + "client_port: 3502\n"
        + "principal_id: 4\n"
        + "        - client_host: 192.168.0.1\n"
        + "client_port: 3503\n"
        + "principal_id: 8\n"
        + "        - client_host: 192.168.0.1\n"
        + "client_port: 3504\n"
        + "principal_id: 12\n"
        + "       - client_host: 192.168.0.1\n"
        + "client_port: 3505\n"
        + "principal_id: 16\n"
        + "replica:\n"
        + "        - principal_id: 0\n"
        + "replica_host: 192.168.0.1\n"
        + "replica_port: 3501\n"
        + "        - enable_histograms_or_summaries: true\n"
        + "time_source_id: time-source1\n"
        + "client_proxy:\n"
        + "        - client_host: 192.168.0.2\n"
        + "client_port: 3502";


    static String clientConfig = "c_val: 0\n"
        + "client_initial_retry_timeout_milli: 150\n"
        + "client_max_retry_timeout_milli: 1000\n"
        + "client_min_retry_timeout_milli: 50\n"
        + "client_number_of_standard_deviations_to_tolerate: 2\n"
        + "client_periodic_reset_thresh: 30\n"
        + "client_proxies_per_replica: 4\n"
        + "client_samples_per_evaluation: 32\n"
        + "client_samples_until_reset: 1000\n"
        + "client_sends_request_to_all_replicas_first_thresh: 4\n"
        + "client_sends_request_to_all_replicas_period_thresh: 2\n"
        + "clients_per_participant_node: 2\n"
        + "comm_to_use: tls\n"
        + "concord-bft_communication_buffer_length: 16777216\n"
        + "enable_mock_comm: false\n"
        + "f_val: 1\n"
        + "num_replicas: 4\n"
        + "num_ro_replicas: 0\n"
        + "prometheus_port: 9873\n"
        + "signing_key_path: resources/signing_keys\n"
        + "tls_certificates_folder_path: /config/daml-ledger-api/config-local/cert\n"
        + "tls_cipher_suite_list: ECDHE-ECDSA-AES256-GCM-SHA384\n"
        + "node:\n"
        + "        - replica:\n"
        + "        - principal_id: 0\n"
        + "replica_host: 192.168.0.1\n"
        + "replica_port: 3501\n"
        + "        - replica:\n"
        + "       - principal_id: 1\n"
        + "replica_host: 192.168.0.2\n"
        + "replica_port: 3501\n"
        + "        - replica:\n"
        + "        - principal_id: 2";


    static List<ConfigurationComponent> componentList = new ArrayList<>();
    static Map<String, Map<String, String>> concordConfig = new HashMap<>();
    static Map<String, String> bftClientConfig = new HashMap<>();
    static IdentityComponentsLists identityComponentsList;
    static Map<String, String> replicaNodeConfig = new HashMap<>();
    List<String> nodes = List.of("fc3b4725-c6c2-49d1-adcb-312156ede59f", "8818735f-873c-47d4-80c5-9c5bf5276524");
    ConfigurationServiceHelper helper =
            new ConfigurationServiceHelper("TelegrafConfigTemplate.conf",
                                           "MetricsConfig.yaml",
                                           "wavefrontConfigTemplate.conf",
                                           "LoggingTemplate.env", true);
    ConcordEcCertificatesGenerator certGen = new ConcordEcCertificatesGenerator();

    @BeforeAll
    static void setup() {
        Security.addProvider(new BouncyCastleProvider());
        ConfigurationComponent logging =
                ConfigurationComponent.newBuilder().setType(ConcordComponent.ServiceType.LOGGING)
                        .setComponentUrl("/fluentd/logging.env").setFilePermissions("rw-------").build();
        ConfigurationComponent telegraf =
                ConfigurationComponent.newBuilder().setType(ConcordComponent.ServiceType.TELEGRAF)
                        .setComponentUrl("/telegraf/telegraf.conf").setComponent(telegrafComponentValue).build();
        ConfigurationComponent damlEngine =
                ConfigurationComponent.newBuilder().setType(ConcordComponent.ServiceType.DAML_EXECUTION_ENGINE)
                        .setComponentUrl("/daml-execution-engine/environment-vars")
                        .setComponent("export JAVA_OPTS=\"-XX:+UseG1GC -Xmx10G\"").build();

        ConfigurationComponent generic =
                ConfigurationComponent.newBuilder().setType(ConcordComponent.ServiceType.GENERIC)
                        .setComponentUrl("/generic/identifiers.env")
                        .setComponent(
                                "NODE_UUID=fc3b4725-c6c2-49d1-adcb-312156ede59f\nBLOCKCHAIN_ID=f84d40ce-e06b-4c93-93d5"
                                + "-8db791cd7d30\nCONSORTIUM_ID=9a1d42db-0fe9-4208-82a5-58368b59a2a9").build();

        componentList.add(logging);
        componentList.add(telegraf);
        componentList.add(damlEngine);
        componentList.add(generic);


        replicaNodeConfig.put("secret", secretConfig);
        replicaNodeConfig.put("deploy", deployConfig);
        concordConfig.put("fc3b4725-c6c2-49d1-adcb-312156ede59f", replicaNodeConfig);

        bftClientConfig.put("8818735f-873c-47d4-80c5-9c5bf5276524", clientConfig);

        IdentityComponent replicaIdentityComponent1 =
                IdentityComponent.newBuilder().setType(IdentityComponent.Type.CERTIFICATE).setBase64Value(certData)
                        .setUrl("file:/concord/config-local/cert/0/client/server.cert").build();
        IdentityComponent replicaIdentityComponent2 =
                IdentityComponent.newBuilder().setType(IdentityComponent.Type.CERTIFICATE).setBase64Value(certData)
                        .setUrl("file:/concord/config-local/cert/0/client/client.cert").build();
        IdentityComponent replicaIdentityComponent3 =
                IdentityComponent.newBuilder().setType(IdentityComponent.Type.KEY).setBase64Value(keyData)
                        .setUrl("file:/concord/config-local/cert/0/client/server.cert").build();
        IdentityComponent replicaIdentityComponent4 =
                IdentityComponent.newBuilder().setType(IdentityComponent.Type.KEY).setBase64Value(keyData)
                        .setUrl("file:/concord/config-local/cert/0/client/client.cert").build();

        IdentityComponent clientIdentityComponent1 =
                IdentityComponent.newBuilder().setType(IdentityComponent.Type.CERTIFICATE).setBase64Value(certData)
                        .setUrl("file:/daml-ledger-api/config-local/cert/0/server/server.cert").build();
        IdentityComponent clientIdentityComponent2 =
                IdentityComponent.newBuilder().setType(IdentityComponent.Type.CERTIFICATE).setBase64Value(certData)
                        .setUrl("file:/daml-ledger-api/config-local/cert/0/server/client.cert").build();
        IdentityComponent clientIdentityComponent3 =
                IdentityComponent.newBuilder().setType(IdentityComponent.Type.KEY).setBase64Value(keyData)
                        .setUrl("file:/daml-ledger-api/config-local/cert/0/server/server.cert").build();
        IdentityComponent clientIdentityComponent4 =
                IdentityComponent.newBuilder().setType(IdentityComponent.Type.KEY).setBase64Value(keyData)
                        .setUrl("file:/daml-ledger-api/config-local/cert/0/server/client.cert").build();

        Map<String, List<IdentityComponent>> replicaIdentityComponentList = Map.of(
                "fc3b4725-c6c2-49d1-adcb-312156ede59f",
                List.of(replicaIdentityComponent1, replicaIdentityComponent2,
                        replicaIdentityComponent3, replicaIdentityComponent4));
        Map<String, List<IdentityComponent>> clientIdentityComponentList = Map.of(
                "8818735f-873c-47d4-80c5-9c5bf5276524",
                List.of(clientIdentityComponent1, clientIdentityComponent2,
                        clientIdentityComponent3, clientIdentityComponent4));

        identityComponentsList = IdentityComponentsLists.builder()
                .concordIdentityComponents(replicaIdentityComponentList)
                .bftIdentityComponents(clientIdentityComponentList)
                .trsIdentityComponents(new HashMap<>())
                .trcIdentityComponents(new HashMap<>())
                .build();

    }

    @AfterAll
    static void teardown() {
        Security.removeProvider("BC");
    }

    @Test
    void testBuildNodeConfigs() {
        nodes.forEach(nodeId -> {
            List<ConfigurationComponent> nodeConfig =
                    helper.buildNodeConfigs(nodeId, componentList, certGen, concordConfig, bftClientConfig,
                            identityComponentsList);
            Assertions.assertTrue(nodeConfig.size() >= 9);
        });
    }

    @Test
    void testBuildNodeConfigsNegative() {
        List<String> nodes = List.of("c75fd941-cc9d-4c4d-ad86-569cfe44452a", "da389b08-a5da-44e6-b278-e65485867dbe");
        nodes.forEach(nodeId -> {
            List<ConfigurationComponent> nodeConfig =
                    helper.buildNodeConfigs(nodeId, componentList, certGen, concordConfig, bftClientConfig,
                            identityComponentsList);
            Assertions.assertTrue(nodeConfig.size() == 4);
        });
    }
}
