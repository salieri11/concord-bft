package profiles;

import static profiles.User.*;

import java.util.Map;
import java.util.Optional;

public class UserPatchRequest {
   private String userID;
   private Optional<String> role = Optional.empty();
   private Optional<String> name = Optional.empty();
   private Optional<String> firstName = Optional.empty();
   private Optional<String> lastName = Optional.empty();
   private Optional<String> email = Optional.empty();

   public UserPatchRequest(String userID, Map<String, String> requestData) {
      this.userID = userID;
      for (Map.Entry<String, String> e : requestData.entrySet()) {
         switch (e.getKey()) {
         case NAME_LABEL:
            this.name = Optional.of(e.getValue());
            break;
         case ROLE_LABEL:
            this.role = Optional.of(e.getValue());
            break;
         case EMAIL_LABEL:
            this.email = Optional.of(e.getValue());
            break;
         case FIRST_NAME_LABEL:
            this.firstName = Optional.of(e.getValue());
            break;
         case LAST_NAME_LABEL:
            this.lastName = Optional.of(e.getValue());
            break;
         }
      }
   }

   public String getUserID() {
      return userID;
   }

   public Optional<String> getRole() {
      return role;
   }

   public void setRole(String role) {
      this.role = Optional.of(role);
   }

   public Optional<String> getFirstName() {
      return firstName;
   }

   public void setFirstName(String firstName) {
      this.firstName = Optional.of(firstName);
   }

   public Optional<String> getLastName() {
      return lastName;
   }

   public void setLastName(String lastName) {
      this.lastName = Optional.of(lastName);
   }

   public Optional<String> getEmail() {
      return email;
   }

   public void setEmail(String email) {
      this.email = Optional.of(email);
   }

   public Optional<String> getName() {
      return name;
   }

   public void setName(Optional<String> name) {
      this.name = name;
   }
}
