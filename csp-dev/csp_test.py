from csp_client import CspClient

def create_users(client, u):
    try:
        client.create_local_user(u["name"], u["password"])
    except requests.HTTPError as err:
        if err.response.status_code == 409:
            print("User {} already exists".format(u["name"]))
            return
        requests.raise_response()

client = CspClient("https://console-stg.cloud.vmware.com", "a3674390-edb6-4c93-915e-daf741909541")
resp = client.create_local_user("admin-blockchain-stg", "GXmryHbKT96twW20iJUr")
print(resp)