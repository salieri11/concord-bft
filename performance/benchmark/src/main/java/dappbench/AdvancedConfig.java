package dappbench;

public class AdvancedConfig {
    private String esxtop;
    private int numberThreads = 0;
    private String concordUsername = "admin@blockchain.local";
    private String concordPassword = "Admin!23";

    public String getEsxtop() {
        return esxtop;
    }

    public void setEsxtop(String esxtop) {
        this.esxtop = esxtop;
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
