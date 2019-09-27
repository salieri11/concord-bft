/* **************************************************************************
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 * **************************************************************************/
package main

import (
    "context" // Use "golang.org/x/net/context" for Golang version <= 1.6
	"crypto/tls"
	"crypto/x509"
	"errors"
	"flag"
	"io/ioutil"
	"net/http"

    "github.com/golang/glog"
    "github.com/grpc-ecosystem/grpc-gateway/runtime"
    "google.golang.org/grpc"
    "google.golang.org/grpc/credentials"

    gw "vmware.com/blockchain/deployment/v1"  // Update
)

var (
    // command-line options:
    // gRPC server endpoint
    grpcServerEndpoint = flag.String("grpc-server-endpoint",  "localhost:9003", "gRPC server endpoint")
    trustedCert = flag.String("trusted-cert", "", "Trusted CA certificate path")
)

func run() error {
    ctx := context.Background()
    ctx, cancel := context.WithCancel(ctx)
    defer cancel()

    // Register gRPC server endpoint
    // Note: Make sure the gRPC server is running properly and accessible
    mux := runtime.NewServeMux(runtime.WithMarshalerOption(runtime.MIMEWildcard, &runtime.JSONPb{OrigName:false}))
    opts := []grpc.DialOption{grpc.WithInsecure()}
    if len(*trustedCert) > 0 {
        bytes, _ := ioutil.ReadFile(*trustedCert)
        certPool := x509.NewCertPool()
        if !certPool.AppendCertsFromPEM(bytes) {
            return errors.New("credentials: Failed to append certificates")
        }
        config := &tls.Config{
            InsecureSkipVerify: false,
            RootCAs: certPool,
        }
        opts = []grpc.DialOption{grpc.WithTransportCredentials(credentials.NewTLS(config))}
    }
    err := gw.RegisterConfigurationServiceHandlerFromEndpoint(ctx, mux,  *grpcServerEndpoint, opts)
    if err != nil {
    return err
    }

    // Start HTTP server (and proxy calls to gRPC server endpoint)
    return http.ListenAndServe(":9083", mux)
}

func main() {
    flag.Parse()
    defer glog.Flush()

    if err := run(); err != nil {
        glog.Fatal(err)
    }
}
