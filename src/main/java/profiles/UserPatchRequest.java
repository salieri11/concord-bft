package profiles;

import java.util.Optional;

public interface UserPatchRequest {
   Long getUserID();

   void setUserID(Long userID);
   
   Optional<String> getOptionalRole();

   Optional<String> getOptionalFirstName();

   Optional<String> getOptionalLastName();

   Optional<String> getOptionalEmail();

   Optional<String> getOptionalName();
   
}
