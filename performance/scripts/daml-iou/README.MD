# Submit IOU

## Overview

[submit.sh](./submit.sh) script can be used as a command line tool to submit command to DAML Ledger API for creating IOU. This script does not require DAML SDK or IOU package to be present on the system from where it is executed. 

## Prerequisites

### Install gRPCurl

[submit.sh](./submit.sh) uses [gRPCurl](https://github.com/fullstorydev/grpcurl) to communicate with the Ledger API. Following commands can be used to install the [latest package](https://github.com/fullstorydev/grpcurl/releases) and make the **grpcurl** command available on the system.

```bash
wget https://github.com/fullstorydev/grpcurl/releases/download/v1.6.1/grpcurl_1.6.1_linux_x86_64.tar.gz
tar -xf grpcurl_1.6.1_linux_x86_64.tar.gz
sudo mv grpcurl /usr/local/bin/
```

### Deploy IOU

Before executing the script, you must ensure that the [quickstart](https://docs.daml.com/getting-started/quickstart.html) app is deployed on the ledger.
So check that using the following SQL on daml_ledger_api DB for the package_id mentioned in [submit.sh](./submit.sh).

```postgresql
SELECT 1 FROM packages WHERE package_id = '<package_id>'
```

Skip rest of the section if the above SQL returns a result. Otherwise, get and deploy the quickstart (IOU) app using the following commands.

```bash
daml new quickstart quickstart-java
cd quickstart
daml deploy --host <ledger-API>
```

[Install DAML SDK](https://docs.daml.com/getting-started/installation.html) prior to using DAML assistant (daml).

```bash
curl -sSL https://get.daml.com/ | sh
add ~/.daml/bin to your PATH
```

Once the app has been deployed, figure out its package UUID using one of the following commands.

```bash
unzip -l  .daml/dist/quickstart-0.0.1.dar | grep quickstart-0.0.1.dalf | awk -F'quickstart-0.0.1-' '{print $2}' | awk -F'/' '{print $1}'
```
OR
```bash
daml damlc inspect-dar .daml/dist/quickstart-0.0.1.dar
```

Update package_id in [submit.sh](./submit.sh) with the newly installed package's UUID obtained from the above command.

## Run

```bash
./submit.sh <ledger-API>
```

[submit.sh](./submit.sh) resolves the variables in [request.json](./request.json) with the corresponding values defined in the script and transmits it to the ledger API using [gRPCurl](https://github.com/fullstorydev/grpcurl) 

Following is a sample output showing that a new transaction has been recorded on the ledger.

```json
{
  "transactionId": "0A0C64616D6C2F33353736333837"
}
```