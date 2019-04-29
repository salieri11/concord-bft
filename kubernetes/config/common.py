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
ARTIFACTORY_KEY="AKCp5cbn4GUnTjgrK31S58ZiykhktwhaofD8LXtcVBfTh7CNbttqzAS1Y6dUmeDWyzH6EUWbX"


AWS_ACCESS_KEY_ID="AKIAI2AOLGCWGGEY53WA"
AWS_SECRET_ACCESS_KEY="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"
AWS_BUCKET="vmbc-saas"

KUBE_STG_CONTEXT="kubernetes-admin@kubernetes"
KUBE_STG_NS="test"
KUBE_STG_CLUSTER_IP="10.152.126.24"
KUBE_PROD_CONTEXT="bsubramanian@vmware.com:/vdp/orgs/vmbc/clusters/res01-stg-us-west-2"
KUBE_PROD_NS="vmbc-prod-saas"
KUBE_PROD_SAAS_URL="vmbc.us-west-2.vdp-stg.vmware.com"
