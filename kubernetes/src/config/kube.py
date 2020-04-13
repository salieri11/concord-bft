
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
			"namespace": "vmbc-stg-saas",
			"saas_url": "vmbc.us-west-2.vdp-stg.vmware.com",
			},
	"production":
			{
			"context": "bsubramanian@vmware.com:/vdp/orgs/vmbc/clusters/res01-prd-us-west-2",
			"namespace": "vmbc-prod-saas",
			"saas_url": "vmbc.vdp.vmware.com",
			}
	}