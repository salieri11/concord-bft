package Contracts;

import java.util.Collections;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class ContractRegistryManager {
    
    private static ContractRegistryManager self = null;
    private static Object lock = new Object();
    
    Map<String, Contract> contractDB;
    
    private ContractRegistryManager() {
        contractDB = new ConcurrentHashMap<>();
    }
    
    public static ContractRegistryManager getInstance() {
        if (self == null) {
            // Make sure this is thread-safe
            synchronized (lock) {
                if (self == null) {
                    self = new ContractRegistryManager();
                }
            }
        }
        return self;
    }
    
    public void addNewContract(Contract c) {
        contractDB.put(c.getAddress(), c);
    }
    
    public boolean hasContract(String address) {
        if (!address.startsWith("0x"))
            address = "0x" + address;
        return contractDB.containsKey(address);
    }
    
    public Contract getContract(String address) {
        if (!address.startsWith("0x"))
            address = "0x" + address;
        return contractDB.get(address);
    }
    
    public Iterator<Contract> allContracts() {
        return Collections.unmodifiableCollection(contractDB.values())
                .iterator();
    }
    
}
