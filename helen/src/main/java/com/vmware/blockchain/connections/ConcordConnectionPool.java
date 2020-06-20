/*
 * Copyright (c) 2018-2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.connections;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.vmware.blockchain.common.BadRequestException;
import com.vmware.blockchain.common.ConcordProperties;
import com.vmware.blockchain.common.ErrorCode;
import com.vmware.blockchain.common.ServiceUnavailableException;
import com.vmware.concord.ConcordTcpConnection;
import com.vmware.concord.IConcordConnection;

import lombok.EqualsAndHashCode;
import lombok.Getter;

/**
 * Connection pool for a specific Blockchain.
 * This has been modified to include the one method remaining in ConnectionPoolFactory.
 */
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class ConcordConnectionPool {
    private static Logger log = LogManager.getLogger(ConcordConnectionPool.class);
    private AtomicInteger connectionCount;
    // should only be incremented a few times (pool size + broken connections)
    private  AtomicInteger nextConnection;
    // initialized with fairness = true, longest waiting threads
    // are served first
    private ArrayBlockingQueue<IConcordConnection> pool;
    private ConcordProperties config;
    private AtomicBoolean initialized;
    // max wait time for pool to return connection
    private int waitTimeout;
    // pool starts at this size
    private int minPoolSize;
    private int maxPoolSize;

    @Getter
    @EqualsAndHashCode.Include
    private UUID id = UUID.randomUUID();

    List<String> ips;
    private ConnectionType connectionType;

    /**
     * Connection types supported.
     */
    public enum ConnectionType {
        TCP, Mock
    }

    /**
     * Initializes local variables.
     */
    public ConcordConnectionPool(List<String> replicaIps, ConnectionType connectionType) {
        initialized = new AtomicBoolean(false);
        connectionCount = new AtomicInteger(0);
        nextConnection = new AtomicInteger(0);
        this.ips = replicaIps;
        this.connectionType = connectionType;
    }

    public ConcordConnectionPool(List<String> replicaIps) {
        this(replicaIps, ConnectionType.TCP);
    }

    public boolean isInitialized() {
        return initialized.get();
    }

    /**
     * Create a connection to the request IP.
     */
    public IConcordConnection create(String nodeIp) throws IOException, UnsupportedOperationException {
        IpAddr ip = new IpAddr(nodeIp);
        switch (connectionType) {
            case TCP:
                ConcordTcpConnection connection =
                    new ConcordTcpConnection(config.getReceiveTimeoutMs(), config.getReceiveHeaderSizeBytes(),
                                             ip.getHost(), ip.getPort());
                return connection;
            case Mock:
                return new MockConnection(ip.getHost(), ip.getPort());
            default:
                throw new BadRequestException(ErrorCode.UNSUPPORTED_TYPE, connectionType);
        }
    }

    /**
     * Creates a new TCP connection with Concord.
     * Use the nextConnection variable to spread the connections around.
     * I'm not convinced this needs to be atomic anymore.
     *
     * @return the connection
     */
    private IConcordConnection createConnection() {
        log.trace("createConnection enter");
        try {
            // increment first, so that all errors can decrement
            int c = connectionCount.incrementAndGet();
            if (c <= maxPoolSize) {
                int i = nextConnection.incrementAndGet() % ips.size();
                IConcordConnection res = create(ips.get(i));
                log.info("new connection created, active connections: " + c);
                return res;
            } else {
                log.debug("pool size at maximum");
                connectionCount.decrementAndGet();
                return null;
            }
        } catch (Exception e) {
            // all exceptions are failures - undo the increment, since we failed
            connectionCount.decrementAndGet();
            log.error("createConnection", e);
            return null;
        } finally {
            log.trace("createConnection exit");
        }
    }

    /**
     * Closes a single connection instance with Concord.
     *
     * @param conn Connection
     */
    private void closeConnection(IConcordConnection conn) {
        log.trace("closeConnection enter");
        try {
            if (conn != null) {
                conn.close();
                int c = connectionCount.decrementAndGet();
                log.debug("connection closed, active connections: " + c);
                log.info("broken connection closed");
            }
        } catch (Exception e) {
            log.error("closeConnection", e);
        } finally {
            log.trace("closeConnection exit");
        }
    }

    /**
     * Removes a connection from the connection pool data structure, checks it, and returns it.
     */
    public IConcordConnection getConnection() throws IOException, InterruptedException {
        log.trace("getConnection enter");

        if (!initialized.get()) {
            throw new ServiceUnavailableException(ErrorCode.CONNECTION_POOL_UNSUPPORTED);
        }

        boolean first = true;
        long start = System.currentTimeMillis();
        while (System.currentTimeMillis() - start < waitTimeout) {
            IConcordConnection conn;

            if (first) {
                // don't wait on the first poll; if the pool is empty, jump
                // immediately to checking if a new connection can be added
                first = false;
                conn = pool.poll();

                if (conn == null) {
                    // this may fail if there are _maxPoolSize connections already
                    conn = createConnection();
                }
            } else {
                // if this is not our first wait, then we weren't allowed to
                // increase the pool size, so we just have to wait for a connection
                conn = pool.poll(waitTimeout, TimeUnit.MILLISECONDS);
            }

            if (conn != null) {
                boolean res = conn.check();
                if (!res) {
                    log.error("Failed to check connection");
                    closeConnection(conn);

                    // attempt to replace the broken connection
                    // this may fail if there are _maxPoolSize
                    // connections already in the pool
                    if (connectionCount.get() < minPoolSize
                        && initialized.get()) {
                        IConcordConnection newConn = createConnection();
                        return newConn;
                    } else {
                        // try to wait for connection to become available
                        continue;
                    }
                }

                log.trace("getConnection exit");
                return conn;
            }
        }

        log.trace("getConnection exit");
        return null;
    }

    /**
     * Adds a connection to the connection pool data structure.
     */
    public void putConnection(IConcordConnection conn) throws IllegalStateException, NullPointerException {
        log.trace("putConnection enter");

        if (!initialized.get()) {
            throw new ServiceUnavailableException(ErrorCode.UNINITIALIZED_POOL);
        }

        // cannot be null in normal flow
        if (conn == null) {
            log.fatal("putConnection, conn is null");
        } else {
            boolean res = pool.offer(conn);

            // cannot fail in normal flow
            if (!res) {
                ((ConcordTcpConnection) conn).close();
                log.fatal("putConnection, pool at maximum");
            }
        }

        log.trace("putConnection exit");
    }

    /**
     * Reads connection pool related configurations.
     */
    public ConcordConnectionPool initialize(ConcordProperties config)
            throws IOException {
        if (initialized.compareAndSet(false, true)) {
            this.config = config;
            waitTimeout = config.getConnectionPoolWaitTimeoutMs();
            minPoolSize = config.getConnectionPoolSize();
            int poolFactor = config.getConnectionPoolFactor();
            maxPoolSize = minPoolSize * poolFactor;

            pool = new ArrayBlockingQueue<IConcordConnection>(maxPoolSize, true);
            for (int i = 0; i < minPoolSize; i++) {
                putConnection(createConnection());
            }

            log.info(String.format("ConcordConnectionPool initialized with %d connections", connectionCount.get()));
        }
        return this;
    }

    /**
     * Closes all connections in the connection pool.
     */
    public void closeAll() {
        initialized.set(false);
        for (IConcordConnection conn : pool) {
            closeConnection(conn);
        }

        pool.clear();
        connectionCount.set(0);
    }

    /**
     * Returns total number of connections.
     */
    public int getTotalConnections() {
        if (!initialized.get()) {
            throw new ServiceUnavailableException(ErrorCode.UNINITIALIZED_POOL);
        }
        return connectionCount.get();
    }

    @Getter
    private static class IpAddr {
        // Default values
        private String host = "concord";
        private int port = 5458;

        public IpAddr(String ipAddr) {
            try {
                // Try with standard url syntax
                URL url = new URL(ipAddr);
                host = url.getHost();
                port = url.getPort();
            } catch (MalformedURLException u) {
                // Assume it's in the form of host:port
                String[] parts = ipAddr.split(":");
                // zero length means a string like ":123"
                if (parts[0].length() > 0) {
                    this.host = parts[0];
                }
                if (parts.length > 1) {
                    try {
                        this.port = Integer.parseInt(parts[1]);
                    } catch (NumberFormatException e) {
                        // do nothing, return default port
                    }
                }
            }
        }
    }
}
