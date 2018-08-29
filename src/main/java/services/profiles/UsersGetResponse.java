/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 */
package services.profiles;

import org.json.simple.JSONObject;

/**
 * A interface which defines the API for response generated for a GET
 * request in User management API
 */
public interface UsersGetResponse {
   JSONObject toJSON();
}
