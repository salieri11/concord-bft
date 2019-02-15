package com.vmware.blockchain.performance;

import com.vmware.blockchain.samples.Ballot;
import org.web3j.crypto.Credentials;
import org.web3j.crypto.WalletUtils;
import org.web3j.protocol.Web3j;
import org.web3j.tx.Contract;
import org.web3j.tx.ManagedTransaction;
import org.web3j.tx.TransactionManager;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class Utils {

    /**
     * load credential from local, if not exist, generates new one
     * @param password
     * @param path
     * @return
     * @throws Exception
     */
    public static Credentials loadCredential(String password, String path) throws Exception{
        File dir = new File(path);
        if (!dir.exists()){
            dir.mkdir();
        }
        File[] files = dir.listFiles();
        if (files.length < 1){
            WalletUtils.generateNewWalletFile(password, dir, true);
        }
        files = dir.listFiles();
        return WalletUtils.loadCredentials(password, files[0]);
    }

    public static Ballot loadContract(Web3j web3j, String path, Credentials credentials) throws Exception{
        Scanner scanner = new Scanner(new File(path));
        String address = scanner.nextLine();
        return Ballot.load(address, web3j, credentials, ManagedTransaction.GAS_PRICE, Contract.GAS_LIMIT);
    }

    public static List<byte[]> getProposals(String path) {
        List<byte[]> proposals = new ArrayList<>();
        try {
            Scanner scanner = new Scanner(new File(path));
            while (scanner.hasNextLine()) {
                String proposalName = scanner.nextLine();
                proposals.add(proposalName.getBytes());
            }
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        return proposals;
    }
}
