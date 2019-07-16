package dappbench;

import java.util.List;

public class AdvancedConfig {
    private List<Object> esxtopCommand = null;
    private int numberThreads = 0;
    private String concordUsername = "admin@blockchain.local";
    private String concordPassword = "Admin!23";

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
}
