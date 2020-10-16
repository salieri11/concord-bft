# Blockchain Node OVF creation using CAP
This provides a self service build of a blockchain node appliance. 
The service uses CAP (Common Appliance Platform) to build ovf used for blockchain node deployment.
This service customizes the ova as required by vmw-blockchain.

## Prerequisites
- You have a VCenter deployed on Nimbus or elsewhere.
NOTE: SDDC does not work with this service.
- You should know the username/password of the VC and the datastore details.
- The datastore should have enough space (atleast 100GB) for a VM to be created.
- The VC should have "VM Network" segment (This is usually default for any VC).
- You have a mac or linux machine to run on.

## Instructions to run
- Download all contents of this folder into any directory of your choice.
- Run "chmod +x startup.sh"
- Run "./startup.sh". Follow through the instructions. Refer to below links for build urls.
startup.sh is an interactive tool, enter values as and when asked. Read the instructions popped for each value asked for.

## Things to remember
- Run "rm -rf cap/" from your working directory between any two consecutive runs.
- You can not provide the same version number for more than one runs if the run has succeeded.
If you indeed need to redo the same ova, log in to your VC and delete the VM created with the name "vmw-blockchain-sh-ovf-<your version number>".

## Does not support/todo
- (TODO) Enable multiple runs with the same installation of CAP.
- (TODO) Creation of content library.
- (TODO) Creation of on-premises nginx content library docker image.
- (Does not support) Creation of content library on SDDCs.

## Useful Links
Refer to https://confluence.eng.vmware.com/pages/viewpage.action?spaceKey=CPDVC&title=CAP+-+Getting+Started for CAP details.

CAP Builds: https://buildweb.eng.vmware.com/ob/?product=cap&branch=develop

CAP Security Hardened ISO Builds: https://buildweb.eng.vmware.com/ob/?product=securityharden-vmbase-generic&branch=cap&buildtype=release

Get dbc account (eases process only): https://confluence.eng.vmware.com/pages/viewpage.action?spaceKey=RDOP&title=DBC+User+Guide#DBCUserGuide-GetyourselfaDBCaccount
Log in to https://my-dbc.eng.vmware.com/ to get account.

Create Nimbus VC: https://confluence.eng.vmware.com/display/CPDVC/Testing+CAP+based+Appliances+on+Nimbus

VCenter GA builds: https://kb.vmware.com/s/article/2143838