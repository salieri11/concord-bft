"""
CSP Client calls for automating csp commands.
"""
import json
from requests import post, delete, patch


class CspClient:
    def __init__(self, csp_url, api_token):
        self.csp_url = csp_url
        self.api_token = api_token

    def get_access_token(self):
        r = post(self.csp_url + "/csp/gateway/am/api/auth/api-tokens/authorize",
                      data={"refresh_token": self.api_token})
        if r.status_code != 200:
            r.raise_for_status()
        return json.loads(r.content)

    def create_local_user(self, user_name, password, first_name="Test", last_name="User"):
        """Create a new local user. Return the userName."""
        url = self.csp_url + "/csp/gateway/am/api/users/local-user"

        body = {
            "firstName": first_name,
            "lastName": last_name,
            "password": password,
            "userName": user_name
        }

        return json.loads(self._do_method(post, url, body))["userName"]

    def create_org(self, display_name):
        """Create an organization with the give name, returns orgId"""
        url = self.csp_url + "/csp/gateway/am/api/orgs"
        body = {"displayName": display_name}

        return json.loads(self._do_method(post, url, body))["refLink"].split("/")[-1]

    def delete_org(self, org_id):
        """Delete the org.  You must use an API Token from this org."""
        url = self.csp_url + "/csp/gateway/am/api/orgs/{id}".format(id=org_id)
        return self._do_method(delete, url)

    def create_service_def(self, parent_id, name):
        """Create a new service from the parent_id.  Return the new service id"""
        url = self.csp_url + "/csp/gateway/slc/api/definitions/{id}/instance".format(id=parent_id)
        body = {"name": name}
        return json.loads(self._do_method(post, url, body))["id"]

    def patch_service_def(self, service_id, patch_body):
        """Use the body to patch the service def.  Normally the serviceUrl.  Returns the reflink"""
        url = self.csp_url + "/csp/gateway/slc/api/definitions/external/{id}".format(id=service_id)
        return json.loads(self._do_method(patch, url, patch_body))["refLink"]

    def delete_service_def(self, service_id):
        url = self.csp_url + "/csp/gateway/slc/api/definitions/external/{id}".format(id=service_id)
        return json.loads(self._do_method(delete, url))["refLink"]

    def create_service_invitations(self, service_id, invitation_count):
        """Create the requested number of invitations to the service.  Returns a list of invitation ids"""
        url = self.csp_url + "/csp/gateway/slc/api/service-invitations"
        body = {
            "isFundingManditory": False,
            "serviceDefinitionLink": "/csp/gateway/slc/api/definitions/external/{id}".format(id=service_id),
            "numberOfInvitationsToGenerate": invitation_count
        }
        response = json.loads(self._do_method(post, url, body))["refLinks"]
        return [id.split("/")[-1] for id in response ]

    def redeem_invitation(self, invitation_id, org_id):
        url = self.csp_url + \
              "/csp/gateway/slc/api/service-invitations/{invitation_id}".format(invitation_id=invitation_id)
        body = {
            "orgLink": "/csp/gateway/am/api/orgs/{org_id}".format(org_id=org_id),
            "invitationAction": "REDEEM"
        }
        return self._do_method(patch, url, body)

    def create_oauth_client(self, org_id, body):
        """Create the oauth client for the given org.  Return map of clientId, clientSecret"""
        url = self.csp_url + "/csp/gateway/am/api/orgs/{org_id}/oauth-apps".format(org_id=org_id)
        return json.loads(self._do_method(post, url, body))

    def create_oauth_default(self, org_id, service_id, display_name, redirect_uris):
        """Create an oauth client with most of the fields defaulted"""
        body = {
            "accessTokenTTL": 1800,
            "allowedScopes": {
                "allRoles": False,
                "organizationScopes": {
                    "allRoles": True,
                },
                "servicesScopes": [
                    {
                        "allRoles": True,
                        "serviceDefinitionId": service_id
                    }
                ]
            },
            "description": display_name,
            "displayName": display_name,
            "grantTypes": [
                "authorization_code",
                "refresh_token"
            ],
            "maxGroupsInIdToken": 20,
            "redirectUris": redirect_uris,
            "refreshTokenTTL": 15552000,
        }

        print(json.dumps(body))

        return self.create_oauth_client(org_id, body)

    def delete_oauth_client(self, org_id, client_id):
        url = self.csp_url + "/csp/gateway/am/api/orgs/{org_id}/oauth-apps".format(org_id=org_id)
        body = {"clientIdsToDelete": [client_id]}
        return json.loads(self._do_method(delete, url, body))

    def _do_method(self, method, url, body = None):
        access_token = self.get_access_token()["access_token"]
        headers = {'csp-auth-token': access_token,
                   'Content-Type': 'application/json'}
        data = json.dumps(body) if body is not None else ""
        r = method(url, headers=headers, data=data)
        if r.status_code != 200:
            r.raise_for_status()
        return r.content

