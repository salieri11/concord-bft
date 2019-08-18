import os
import sys

BINTRAY_KEY="0ee8cfa59bb2a5c1e8c7697a6670a0bf919decfd"
BINTRAY_USER="bsubramanian@vmware"
BINTRAY_REPO="blockchainsaas"
BINTRAY_REPO_URL="vmware-docker-blockchainsaas.bintray.io"
BINTRAY_API="https://api.bintray.com/"
BINTRAY_TAG="latest"

BC_PACKAGES={
            "saas": ["ui", "contract-compiler", "persephone-provisioning",
                    "persephone-metadata", "helen"],
            "blockchain": ["concord-core", "agent", "ethrpc"]
            }

ARTIFACTORY_API="https://build-artifactory.eng.vmware.com/artifactory/api"
ARTIFACTORY_REPO="athena-docker-local"
ARTIFACTORY_REPO_URL="%s.artifactory.eng.vmware.com" % (ARTIFACTORY_REPO)
ARTIFACTORY_USER="bsubramanian"
ARTIFACTORY_KEY="<ARTI_KEY>"


AWS_ACCESS_KEY_ID="AKIAI2AOLGCWGGEY53WA"
AWS_SECRET_ACCESS_KEY="<AWS_ACCESS_KEY>"
AWS_BUCKET="vmbc-saas"


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
