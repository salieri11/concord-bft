package dappbench;

import java.util.List;

public class AdvancedConfig {
    private Wavefront wavefront = null;
    private List<Object> esxtopCommand = null;
    private int numberThreads = 0;
    private String concordUsername = "admin@blockchain.local";
    private String concordPassword = "Admin!23";
    private boolean dockerStats = false;
    private boolean esxTop = false;

    public Wavefront getWavefront() { return wavefront; }

    public void setWavefront(Wavefront wavefront) { this.wavefront = wavefront; }

    public List<Object> getEsxtopCommand() {
        return esxtopCommand;
    }

    public void setEsxtopCommand(List<Object> esxtopCommand) {
        this.esxtopCommand = esxtopCommand;
    }

    public int getNumberThreads() {
    	return numberThreads;
    }
    
    public void setNumberThreads(int threads) {
    	this.numberThreads = threads;
    }
    
    public String getConcordUsername() {
    	return concordUsername;
    }
    
    public void setConcordUsername(String username) {
    	this.concordUsername = username;
    }
    
    public String getConcordPassword() {
    	return concordPassword;
    }
    
    public void setConcordPassword(String password) {
    	this.concordPassword = password;
    }

    public void setDockerStats(boolean dockerStats) {
        this.dockerStats = dockerStats;
    }

    public boolean isDockerStats() {
        return dockerStats;
    }

    public boolean getEsxTop() {
        return esxTop;
    }

    public void setEsxTop(boolean esxTop) {
        this.esxTop = esxTop;
    }

    public static class Wavefront {

        private boolean enabled;
        private String proxyHost;
        private int metricsPort;
        private String source;

        public boolean isEnabled() {return  enabled; }
        public void setEnabled(boolean enabled) { this.enabled = enabled; }

        public String getProxyHost() { return proxyHost; }
        public void setProxyHost(String proxyHost) { this.proxyHost = proxyHost; }

        public int getMetricsPort() { return metricsPort; }
        public void setMetricsPort(int metricsPort) { this.metricsPort = metricsPort; }

        public String getSource() { return source; }
        public void setSource(String source) { this.source = source; }
    }
}
