'''
Do all the steps required to set up a CSP dev environment, driven from an input yaml file
'''
import argparse
import requests
import yaml

import csp_client


def create_users(client, u):
    try:
        client.create_local_user(u["name"], u["password"])
    except requests.HTTPError as err:
        if err.response.status_code == 409:
            print("User {} already exists".format(u["name"]))
            return
        requests.raise_response()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("file", help="YAML input file")
    args = parser.parse_args()
    with open(args.file, "r") as stream:
        setup = yaml.safe_load(stream)
    # get the base information we need
    base = setup["base"]
    service_org = base["service_owner_org_id"]
    client = csp_client.CspClient(base["csp_url"], base["service_owner_token"])
    serviceName = setup["create_service"]["name"]
    # create the service definition
    service_id = client.create_service_def(base["service_template_id"], serviceName)
    serviceDict = {serviceName: service_id}
    print("Service {}: id{}".format(serviceName, service_id))
    # patch it to pick up the redirect
    resp = client.patch_service_def(serviceDict[setup["patch_service"]["service"]], setup["patch_service"]["body"])
    print("Patch: {}".format(resp))
    # create an invitation.  Note this returns a list
    invitations = client.create_service_invitations(service_id, 1)
    print("Invitations: {}".format(invitations))
    # and redeem it
    resp = client.redeem_invitation(invitations[0], service_org)
    print("Accept invite {}".format(resp))
    #create oauth client
    oauth = setup["create_oauth"]
    resp = client.create_oauth_default(service_org, service_id, oauth["name"],
                                       oauth["redirect_uris"])
    print(resp)

    # now print out the info needed

    # CSP values
    print("csp.url={}".format(base["csp_url"]))
    print("vmbc.url.public={}".format(setup["vmbc_url"]))
    print("vmbc.client.id={}".format(resp["clientId"]))
    print("vmbc.client.secret={}".format(resp["clientSecret"]))
    print("vmbc.service.id={}".format(service_id))
    print("vmbc.csp.service.user.refresh.token={}".format(base["service_owner_token"]))
    print("vmbc.auth.csp=true")



if __name__ == "__main__":
    main()

