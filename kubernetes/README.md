# Kubernetes setup for SAAS deployment of VMware Blockchain

File structure:
---------------
1. kubenetes/config houses all the configs and secrets(for now) to run the deployments
* kube/kubeconfig - kubectl config for staging and production
* common.py - bintray, artifactory and k8 namespaces configs
* helen - houses all files needed for helen bringup. schema will be setup as a configmap,
          and entrypoint.sh for the container.
* persephone - deployment service config.json for sddc and container repository endpoints

Requirements:
-------------
1. To run kubernetes deployments kubectl is required to run the manifests
2. kubeconfig for staging and production k8 environments.
3. Vmware artifactory and bintray secrets loaded to K8 namespaces

Staging:
-------
* Staging environment is bare metal kubernetes cluster with two nodes.
* CNI is setup with flannel
DEFAULT NAMESPACE - "test"

Production:
-----------
* Production environment is a namespace backed kubernetes cluster setup by VDP on AWS
DEFAULT NAMESPACE - "vmbc-prod-saas"


Prereqs for saas deployments: (TODO-automate configmap setup)
-----------------------------
1. For helen create a configmap named helenmap 
>cd kubernetes
>kubectl -n <NAMESPACE> create configmap helenmap --from-file=../config/helen/
2. For fleet management with provisioning service create a configmap named fleetmap
>cd kubernetes
>kubectl -n <NAMESPACE> create configmap fleetmap --from-file=config/persephone/persephone/provision-service/


Fleet-management on K8 (env - stagingconfigs/productionconfigs)
----------------------
* kubectl -n <NAMESPACE> create -f config/kube/<env>/fleet-mgmt-deployment.yaml
* kubectl -n <NAMESPACE> create -f config/kube/<env>/fleet-mgmt-service.yaml

SAAS deployments on K8 (env - stagingconfigs/productionconfigs)
----------------------
* kubectl -n <NAMESPACE> create -f config/kube/<env>/saas-app-deployment.yaml
* kubectl -n <NAMESPACE> create -f config/kube/<env>/saas-app-service.yaml

Access to services from external network:
-----------------------------------------

On baremetal staging nodeport will be used to expose app's service endpoints via controller cluster ip
On AWS K8 ingress rules will be setup to access the service via FQDN

Staging - 
--------
Fleet management will be accessible by default on port 30092
UI and Helen will be accessible by default on port 30081

Production -
----------
Setting up ingress rules on AWS environment to resolve dns for the services
SAAS * kubectl -n <NAMESPACE> create -f productionconfigs/saas-ingress.yaml
Once resolved the ui should be accessible via vmbc.us-west-2.vdp-stg.vmware.com





