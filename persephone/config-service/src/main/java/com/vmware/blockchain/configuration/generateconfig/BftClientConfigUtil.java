/*
 * Copyright (c) 2020 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.configuration.generateconfig;

import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.yaml.snakeyaml.Yaml;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * Utility class for generating the input for Configuration Yaml file.
 */
public class BftClientConfigUtil {

    private static final Logger log = LoggerFactory.getLogger(BftClientConfigUtil.class);

    private String bftClientConfigTemplatePath;

    public int maxPrincipalId;

    public BftClientConfigUtil(String bftClientConfigTemplatePath) {
        this.bftClientConfigTemplatePath = bftClientConfigTemplatePath;
    }

    /**
     * file path.
     */
    public static final String configPath = "/daml-ledger-api/config-public/bftclient.config";

    /**
     * persistence.
     */
    public final Map<Integer, List<Integer>> nodePrincipal = new HashMap<>();

    /**
     * Utility to generate concord config.
     */
    public Map<String, String> getbftClientConfig(List<String> nodeIds, List<String> hostIps,
                                                  List<String> participantIps) {
        try {
            var result = new HashMap<String, String>();

            var outputPath = Files.createTempDirectory(null);
            var principalsMapFile = Paths.get(outputPath.toString(), "principals.json").toString();
            var inputYamlPath = Paths.get(outputPath.toString(), "dockerConfigurationInput.yaml").toString();

            generateConfigYaml(hostIps, participantIps, inputYamlPath);

            // TODO: Dependency on config-gen tool can be avoided here.
            // Config gen tool is only giving the principal ids
            var configFuture = new ProcessBuilder("/app/conc_genconfig",
                    "--configuration-input",
                    inputYamlPath,
                    "--report-principal-locations",
                    principalsMapFile,
                    "--client-conf",
                    "true")
                    .directory(outputPath.toFile())
                    .start()
                    .onExit();

            var work = configFuture.whenCompleteAsync((process, error) -> {
                if (error == null) {
                    try {
                        var principalStr = Files.readString(Path.of(principalsMapFile));
                        if (principalStr.isBlank() || principalStr.isEmpty()) {
                            throw new Exception("PrincipalIds are not generated by concord config");
                        }

                        TypeReference<Map<Integer, List<Integer>>> typeRef
                                = new TypeReference<>() {};

                        ObjectMapper objectMapper = new ObjectMapper();
                        objectMapper.configure(MapperFeature.ACCEPT_CASE_INSENSITIVE_PROPERTIES, true);
                        var principalsMap = objectMapper.readValue(principalStr, typeRef);
                        principalsMap.forEach((key, value) -> nodePrincipal.putIfAbsent(key - 1, value));


                        for (int num = 0; num < participantIps.size(); num++) {
                            var path = outputPath.resolve("Participant" + (num) + ".config");
                            result.put(nodeIds.get(num), Files.readString(path));
                        }
                    } catch (Throwable collectError) {
                        log.error("Cannot collect generated cluster configuration",
                                collectError);
                    }
                } else {
                    log.error("Cannot run config generation process", error);
                }
            });

            work.join();
            return result;

        } catch (IOException e) {
            log.error("Exception while generating concord config {}", e.getLocalizedMessage());
            return null;
        }
    }

    /**
     * Utility method for generating input config yaml file.
     */
    boolean generateConfigYaml(List<String> hostIps, List<String> participantIps, String configYamlPath) {
        if (!ConfigUtilHelpers.validateSbft(hostIps)) {
            return false;
        }
        int clusterSize = hostIps.size();
        int fVal = ConfigUtilHelpers.getFVal(clusterSize);
        int cVal = ConfigUtilHelpers.getCVal(clusterSize, fVal);
        return generateConfigYaml(hostIps, participantIps, fVal, cVal, configYamlPath);
    }

    /**
     * Utility method for generating input config yaml file.
     */
    @SuppressWarnings({"unchecked"})
    boolean generateConfigYaml(List<String> hostIp, List<String> participantIps,
                               int fVal, int cVal, String configYamlPath) {

        var maxCommitterPrincipalId = (hostIp.size()
                + ConfigUtilHelpers.CLIENT_PROXY_PER_COMMITTER * hostIp.size()) - 1;
        maxPrincipalId = maxCommitterPrincipalId + ((participantIps.size()
                + ConfigUtilHelpers.CLIENT_PROXY_PER_PARTICIPANT * participantIps.size()) - 1);

        if (!ConfigUtilHelpers.validateSbft(hostIp, fVal, cVal)) {
            return false;
        }

        Path path = Paths.get(configYamlPath);

        Yaml yaml = new Yaml();
        Map<String, Object> configInput;
        try {
            configInput = yaml.load(new FileInputStream(bftClientConfigTemplatePath));
        } catch (FileNotFoundException e) {
            // For unit tests only.
            log.warn("File {} does not exist: {}\n Using localized config yaml input template",
                    bftClientConfigTemplatePath, e.getLocalizedMessage());
            ClassLoader classLoader = getClass().getClassLoader();
            configInput = yaml.load(classLoader.getResourceAsStream("BFTClientConfigTemplate.yaml"));
        }

        // Add generic configs
        configInput.put(ConfigUtilHelpers.ConfigProperty.F_VAL.name, fVal);
        configInput.put(ConfigUtilHelpers.ConfigProperty.C_VAL.name, cVal);
        configInput.put(ConfigUtilHelpers.ConfigProperty.NUM_PARTICIPANTS.name, participantIps.size());

        // Prepare per replica config
        List node = (List) configInput.get(ConfigUtilHelpers.ConfigProperty.NODE.name);
        Map<String, Object> nodeConfig = (Map<String, Object>) node.get(0);

        List replicaConfig = (List) nodeConfig.get(ConfigUtilHelpers.ConfigProperty.REPLICA.name);
        Map<String, Object> replicaValues = (Map<String, Object>) replicaConfig.get(0);

        List resultNodes = new ArrayList();

        int counter = 0;
        for (String s : hostIp) {
            replicaValues.put(ConfigUtilHelpers.ConfigProperty.REPLICA_HOST.name, s);
            replicaValues.put(ConfigUtilHelpers.ConfigProperty.COMMITTER_PORT.name,
                    ConfigUtilHelpers.DEFAULT_PORT + counter);
            nodeConfig.put(ConfigUtilHelpers.ConfigProperty.REPLICA.name,
                    new ArrayList(Collections.singletonList(ConfigUtilHelpers.clone(replicaValues))));
            resultNodes.add(ConfigUtilHelpers.clone(nodeConfig));
            counter = counter + 1;
        }

        configInput.put(ConfigUtilHelpers.ConfigProperty.NODE.name, resultNodes);

        // Prepare participant configs
        List participantNodes = (List) configInput.get(ConfigUtilHelpers.ConfigProperty.PARTICIPANT_NODES.name);
        Map<String, Object> participantNodesConfig = (Map<String, Object>) participantNodes.get(0);
        List participantNode = (List) participantNodesConfig.get(
                ConfigUtilHelpers.ConfigProperty.PARTICIPANT_NODE.name);
        Map<String, Object> participantConfig = (Map<String, Object>) participantNode.get(0);

        List externalClients = (List) participantConfig.get(ConfigUtilHelpers.ConfigProperty.EXTERNAL_CLIENTS.name);
        Map<String, Object> clientValues = (Map<String, Object>) externalClients.get(0);
        List client = (List) clientValues.get(ConfigUtilHelpers.ConfigProperty.CLIENT.name);
        Map<String, Object> clientPort = (Map<String, Object>) client.get(0);

        List participantNodeRes = new ArrayList();
        Map<String, Object> participantNodeResMap = new HashMap<>();
        for (String s : participantIps) {
            List resultClients = new ArrayList();
            var participantConfigRes = ConfigUtilHelpers.clone(participantConfig);
            participantConfigRes.put(ConfigUtilHelpers.ConfigProperty.PARTICIPANT_NODE_HOST.name, s);

            for (int j = 0; j < ConfigUtilHelpers.CLIENT_PROXY_PER_PARTICIPANT; j++) {
                var port = ConfigUtilHelpers.DEFAULT_PORT + counter + j;
                clientPort.put(ConfigUtilHelpers.ConfigProperty.CLIENT_PORT.name, port);
                clientValues.put(ConfigUtilHelpers.ConfigProperty.CLIENT.name,
                        new ArrayList(Collections.singletonList(ConfigUtilHelpers.clone(clientPort))));
                resultClients.add(ConfigUtilHelpers.clone(clientValues));
            }
            counter = counter + ConfigUtilHelpers.CLIENT_PROXY_PER_PARTICIPANT;

            participantConfigRes.put(ConfigUtilHelpers.ConfigProperty.EXTERNAL_CLIENTS.name, resultClients);
            participantNodeResMap.put(ConfigUtilHelpers.ConfigProperty.PARTICIPANT_NODE.name,
                    new ArrayList(Collections.singletonList(ConfigUtilHelpers.clone(participantConfigRes))));
            participantNodeRes.add(ConfigUtilHelpers.clone(participantNodeResMap));
        }

        configInput.put(ConfigUtilHelpers.ConfigProperty.PARTICIPANT_NODES.name, participantNodeRes);

        try {
            BufferedWriter writer = Files.newBufferedWriter(path);
            yaml.dump(configInput, writer);
            writer.flush();
            writer.close();
            return true;
        } catch (IOException x) {
            log.error("IOException: %s%n", x);
            return false;
        }
    }

}
