import os

AWS_BUCKET="vmbc-saas"

BC_COMPONENTS={
            "saas": ["ui", "contract-compiler", "persephone-provisioning",
                    "helen", "persephone-configuration",
                    "persephone-ipam", "fluentd"],
            "blockchain": ["concord-core", "agent", "ethrpc", "daml-ledger-api",
                    "daml-execution-engine", "daml-index-db", "committer-tools"]
            }

CSP_PROD = "https://console.cloud.vmware.com/csp/gateway"
CSP_STG = "https://console-stg.cloud.vmware.com/csp/gateway"

CSP_PROD_KEY = "PRODUCTION_API_TOKEN"
CSP_STG_KEY = "STAGING_API_TOKEN"

IPAM_URL = "ipam-vmbc.cloud.vmware.com:443"

VAULT_ENDPOINT = "http://10.78.20.9:8200"
VAULT_KEY = os.environ.get("VAULT_KEY", None)

SLACK_CHANNELS = ["vdaml-devops", "slackbot"]

VAULT_KNOWN_KEYS = ["VMC", "slack", "artifactory", "bintray",
					"ZONE-SDDC-MAP", "SDDC-FOLDER-MAP", "ipam", "helendb"]

PRODUCTION_SDDCS = ["VMware-Blockchain-SDDC-2", "VMware-Blockchain-SDDC-3"]



