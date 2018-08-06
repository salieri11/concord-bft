package profiles;

import java.util.Optional;

public interface UserCreateRequest {
   String getUserName();

   Optional<String> getOptionalFirstName();

   Optional<String> getOptionalLastName();

   String getEmail();

   String getRole();

   String getPassword();
   
   Long getOrganizationID();
   
   Long getConsortiumID();
}
