# Before you begin

Install gcloud tooling on your machine:
https://cloud.google.com/sdk/install

Make sure that you have a user account defined in GCP and you have access to the 
`da-dev-concord2` project. If you have never done it, you should browse e.g.
https://console.cloud.google.com/ from your DA Google account and accept terms &
conditions. Make sure you are connected to the `GCP-Frankfurt-FT` VPN and check
if you have access by running this command:

```gcloud beta compute --project "da-dev-concord2" ssh --zone "europe-west6-a" "da-dev-concord2-1"```

If it fails, you should contact helpdesk to obtain access. You will also have to
run this command at least once daily to refresh the access tokens. After that you
can also connect using ssh:

```ssh <GCP user name>_digitalasset_com@34.65.169.49```

You must configure your docker for GCR access on your local machine:

```gcloud auth configure-docker```

The administrator of the GCP VM instance will have already configured GCR access
there in a similar manner.

# Deploying DAML on VMware into GCP engironment

The `daml/gcp/deploy.sh` script can be used to deploy docker images, environment
variables as well as scripts necessary to run CHESS+ tests. This script is meant
be used from the developer machine.

Let the script know your target GCP instance with:

```export GCP_TARGET="<GCP user name>_digitalasset_com@34.65.169.49"```

then launch the script from the main `vmwathena_blockchain` directory
without command line options to get help:

```vmwathena_blockchain> daml/gcp/deploy.sh```

This script can also be used to initiate a GCP test from the developer machine using:

```vmwathena_blockchain> daml/gcp/deploy.sh scripts && daml/gcp/deploy.sh test```

For more information consult the help screen of the `deploy.sh` script itself
using an undefined command line argument or `--help`.

# Testing DAML on VMware in GCP environment

The `daml/gcp/load-runner.sh` script can be used for running the CHESS+ tests.
This script is meant be used from the remote GCP machine.

The script assumes that the docker `latest` images required to run DAML on VMware
are all present in the GCP machine''s' docker repository. Use the `deploy.sh images`
functionality described above to send those images.

Before running the script also make sure to login to docker:

```docker login```

Provide user credentials that give you authorization to pull private spider
artifacts from docker hub. If your user credentials don't allow you to do it,
contact DA's helpdesk.

Launch the script from the main `vmwathena_blockchain` directory:

```vmwathena_blockchain> daml/gcp/load-runner.sh test```

For more information consult the help screen of the `load-runner.sh` script itself
using an undefined command line argument or `--help`.
