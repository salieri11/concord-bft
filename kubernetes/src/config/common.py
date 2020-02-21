import os
import sys

ARTIFACTORY_API="https://build-artifactory.eng.vmware.com/artifactory/api"
ARTIFACTORY_REPO="athena-docker-local"
ARTIFACTORY_REPO_URL="%s.artifactory.eng.vmware.com" % (ARTIFACTORY_REPO)
ARTIFACTORY_USER="bsubramanian"

AWS_BUCKET="vmbc-saas"

BINTRAY_USER="bsubramanian@vmware"
BINTRAY_REPO="blockchainsaas"
BINTRAY_SUBJECT="vmware"
BINTRAY_REPO_URL="vmware-docker-blockchainsaas.bintray.io"
BINTRAY_API="https://api.bintray.com/"
BINTRAY_COMMIT_LABEL = "docker_label_com_vmware_blockchain_commit"
BINTRAY_BUILD_LABEL = "docker_label_com_vmware_blockchain_version"


BC_COMPONENTS={
            "saas": ["ui", "contract-compiler", "persephone-provisioning",
                    "helen", "persephone-configuration",
                    "persephone-ipam", "fluentd"],
            "blockchain": ["concord-core", "agent", "ethrpc", "daml-ledger-api",
                    "daml-execution-engine", "daml-index-db",
					"fabric-tools", "fabric-peer", "fabric-orderer",
					"hlf-tools", "hlf-peer", "hlf-orderer" ]
            }

CSP_PROD = "https://console.cloud.vmware.com/csp/gateway"
CSP_STG = "https://console-stg.cloud.vmware.com/csp/gateway"

GIT_API = "https://git.eng.vmware.com/desperado/api/repos"
GITLAB_API = "https://gitlab.eng.vmware.com/api/v4/"
GITLAB_DEFAULT_GROUP = "blockchain"
GITLAB_DEFAULT_REPO = "vmwathena_blockchain"

IPAM_URL = "ipam-vmbc.cloud.vmware.com:443"

VAULT_ENDPOINT = "http://10.78.20.9:8200"
VAULT_KEY = os.environ.get("VAULT_KEY", None)


KUBE_CONFIGS = {
	"local":
	        {
	        "context": "kubernetes-admin@kubernetes",
			"namespace": "test",
			"cluster_ip": "10.78.20.2",
		},
	"staging":
			{
			"context": "bsubramanian@vmware.com:/vdp/orgs/vmbc/clusters/res01-stg-us-west-2",
			"namespace": "vmbc-prod-saas",
			"saas_url": "vmbc.us-west-2.vdp-stg.vmware.com",
			},
	"production":
			{
			"context": "bsubramanian@vmware.com:/vdp/orgs/vmbc/clusters/res01-prd-us-west-2",
			"namespace": "vmbc-prod-saas",
			"saas_url": "vmbc.vdp.vmware.com",
			}
	}

#path to configs kube configs relative to this file
K8_CONFDIR="kube"
K8_CONF_PATH=os.path.join(os.path.dirname(os.path.abspath(__file__)), K8_CONFDIR)
K8_PROD_CONF=os.path.join(K8_CONF_PATH, "production")
K8_STG_CONF=os.path.join(K8_CONF_PATH, "staging")
