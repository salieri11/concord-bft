'''
Do all the steps required to set up a CSP dev environment, driven from an input yaml file
'''
import argparse
import yaml

import csp_client


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("file", help="YAML input file")
    args = parser.parse_args()
    with open(args.file, "r") as stream:
        cleanup = yaml.safe_load(stream)
    # get the base information we need
    base = cleanup["base"]
    service_org = base["service_owner_org_id"]
    client = csp_client.CspClient(base["csp_url"], base["service_owner_token"])
    resp = client.delete_oauth_client(service_org, cleanup["client_id"])
    print(resp)
    resp = client.delete_service_def(cleanup["service_id"])
    print(resp)



if __name__ == "__main__":
    main()

