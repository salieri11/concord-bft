Deploy docker-compose-daml-with-tls.yml

    Command: docker-compose -f docker-compose-daml-with-tls.yml up -d
    P.S. Delete devdata before deployment

After ensuring all the containers are up and running, try out the following commands to verify:

    Negative case(trying to connect without TLS certificates)
        Command: daml ledger list-parties --host localhost --port 6861 

        Expected output:
            Listing parties at localhost:6861
            daml-helper: GRPCIOBadStatusCode StatusUnavailable (StatusDetails {unStatusDetails = "Socket closed"})

    Positive case(trying to connect with valid TLS certificates)
        Command: daml ledger list-parties --host localhost --port 6861 --pem client.key --crt client.crt --cacrt root-ca.crt 

        Expected output:
            Listing parties at localhost:6861
            no parties are known