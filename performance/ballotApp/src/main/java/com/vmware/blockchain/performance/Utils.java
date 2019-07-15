package com.vmware.blockchain.performance;

import com.vmware.blockchain.samples.Ballot;

import okhttp3.OkHttpClient;
import okhttp3.OkHttpClient.Builder;

import org.web3j.crypto.Credentials;
import org.web3j.crypto.WalletUtils;
import org.web3j.protocol.Web3j;
import org.web3j.tx.gas.DefaultGasProvider;

import java.io.File;
import java.io.FileNotFoundException;

import java.security.cert.X509Certificate;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.concurrent.TimeUnit;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSession;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

public class Utils {

	/**
	 * load credential from local, if not exist, generates new one
	 * @param password
	 * @param path
	 * @return
	 * @throws Exception
	 */
	public static Credentials loadCredential(String password, String path) throws Exception {
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

	public static Ballot loadContract(Web3j web3j, String path, Credentials credentials) throws Exception {
		Scanner scanner = new Scanner(new File(path));
		String address = scanner.nextLine();
		scanner.close();
		return Ballot.load(address, web3j, credentials, DefaultGasProvider.GAS_PRICE, DefaultGasProvider.GAS_LIMIT);
	}

	public static List<byte[]> getProposals(String path) {
		List<byte[]> proposals = new ArrayList<>();
		Scanner scanner = null;
		try {
			scanner = new Scanner(new File(path));
			while (scanner.hasNextLine()) {
				String proposalName = scanner.nextLine();
				proposals.add(proposalName.getBytes());
			}
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} finally {
			scanner.close();
		}
		return proposals;
	}

	public static OkHttpClient getClient() throws Exception {
		TrustManager[] trustAllCerts = new TrustManager[]{
				new X509TrustManager() {
					public java.security.cert.X509Certificate[] getAcceptedIssuers() {
						return new X509Certificate[0];
					}
					public void checkClientTrusted(
							java.security.cert.X509Certificate[] certs, String authType) {
					}
					public void checkServerTrusted(
							java.security.cert.X509Certificate[] certs, String authType) {
					}
				}
		};

		// Install the all-trusting trust manager
		SSLContext sc = null;
		try {
			sc = SSLContext.getInstance("SSL");
			sc.init(null, trustAllCerts, new java.security.SecureRandom());
			HttpsURLConnection.setDefaultSSLSocketFactory(sc.getSocketFactory());
		} catch (Exception e) {
			e.printStackTrace();
		}

		Builder builder = new Builder();
		builder.connectTimeout(0, TimeUnit.MILLISECONDS);
		builder.readTimeout(0, TimeUnit.MILLISECONDS);
		builder.writeTimeout(0, TimeUnit.MILLISECONDS);
		builder.retryOnConnectionFailure(true);
		builder.sslSocketFactory(sc.getSocketFactory(), (X509TrustManager)trustAllCerts[0]);

		builder.hostnameVerifier(new HostnameVerifier() {
			@Override
			public boolean verify(String hostname, SSLSession session) {
				return true;
			}
		});

		if(BallotDApp.ENABLE_LOGGING) {
			builder.addNetworkInterceptor(BallotDApp.getLoggingInterceptor());
		}

		OkHttpClient client = null;
		try {
			client = builder.build();
			return client;
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}
}
