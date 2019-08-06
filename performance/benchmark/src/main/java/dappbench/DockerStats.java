package dappbench;

import com.jcraft.jsch.*;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.io.InputStream;
import java.util.*;

public class DockerStats implements Runnable {
    private static final Logger logger = LogManager.getLogger(DockerStats.class);
    private String nodeIP;
    private Properties config;
    private JSch jsch;
    private Session session;
    private String command;
    private Map<String, List<String> > dockerTable;

    public DockerStats(Node node, Map<String, List<String> > dockerTable) {
        try {
            this.dockerTable = dockerTable;
            this.nodeIP = node.ip;
            config = new Properties();
            config.put("StrictHostKeyChecking", "no");
            jsch = new JSch();
            session = jsch.getSession(node.username, nodeIP, 22);
            session.setPassword(node.password);
            session.setConfig(config);
            // Establish the connection
            session.connect();
            command = "docker stats --no-stream --format \"table {{.Name}},{{.CPUPerc}},{{.MemUsage}},{{.MemPerc}}\"";
        }catch(JSchException jschX) {
            logger.warn(jschX.getMessage());
        }
    }

    public void run() {
        StringBuilder outputBuffer = new StringBuilder();
        try {
            //Running the command to get docker stats
            ChannelExec channel =  (ChannelExec)session.openChannel("exec");
            channel.setCommand(command);
            InputStream commandOutput = channel.getInputStream();
            channel.connect();
            int readByte = commandOutput.read();

            while(readByte != 0xffffffff)
            {
                outputBuffer.append((char)readByte);
                readByte = commandOutput.read();
            }
            channel.disconnect();
            String[] statsTable = outputBuffer.toString().split("\n");
            for(int i = 1; i < statsTable.length; i++) {
                String[] stats = statsTable[i].split(",");
                if (dockerTable.containsKey(stats[0])) {
                    String cpuPerc = dockerTable.get(stats[0]).get(1), memUsage = dockerTable.get(stats[0]).get(2), memPerc = dockerTable.get(stats[0]).get(3);
                    if (cpuPerc.compareTo(stats[1]) < 0)
                        cpuPerc = stats[1];
                    if (memUsage.compareTo(stats[2]) < 0)
                        memUsage = stats[2];
                    if (memPerc.compareTo(stats[3]) < 0)
                        memPerc = stats[3];
                    dockerTable.put(stats[0], Arrays.asList(stats[0], cpuPerc, memUsage, memPerc));
                } else {
                    dockerTable.put(stats[0], Arrays.asList(stats[0], stats[1], stats[2], stats[3]));
                }
            }
        } catch (JSchException | IOException jschX) {
            logger.warn(jschX.getMessage());
        }
    }
}
