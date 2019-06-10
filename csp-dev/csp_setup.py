'''
Do all the steps required to set up a CSP dev environment, driven from an input yaml file
'''
import argparse
import requests
import yaml

import csp_client


def create_users(client, user_info):
    for u in user_info:
        try:
            client.create_local_user(u["name"], u["password"])
        except requests.HTTPError as err:


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("file", help="YAML input file")
    args = parser.parse_args()
    with open(args.file, "r") as stream:
        setup = yaml.safe_load(stream)
    # get the base information we need
    base = setup["base"]
    client = csp_client.CspClient(base["csp_url"], base["service_owner_token"])






if __name__ == "__main__":
    main()

