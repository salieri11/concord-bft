#########################################################################
# Copyright 2018 - 2019 VMware, Inc.  All rights reserved. -- VMware Confidential
#
# This contains methods related to things like verifying a rest call
# response.
#########################################################################

def validateAccessDeniedResponse(result, expectedPath):
   '''
   Validates the returned result of an Access Denied error.
   The error code, error message, and status are the same.
   Accepts the result to evaluate and the expected value for "path".
   Path is the URL path.  e.g. "/api/consortiums"
   '''
   for field in ["error_code", "error_message", "status", "path"]:
      assert field in result, "Expected field '{}' in {}".format(field, result)

   assert result["error_code"] == "AccessDeniedException", "Expected different error code."
   assert result["error_message"] == "Access is denied", "Expected different error message."
   assert result["status"] == 403, "Expected HTTP status 403."
   assert result["path"] == expectedPath, "Expected different path."
