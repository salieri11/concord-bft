package profiles;

import java.util.Map;
import java.util.Optional;
import static profiles.Consortium.CONSORTIUM_ID_LABEL;
import static profiles.Organization.ORGANIZATION_ID_LABEL;
import static profiles.User.*;
import static profiles.UserRole.ROLE_LABEL;

public class UserPatchRequest {
   
   private String userID;
   private Optional<Roles> role;
   private Optional<String> consortiumID;
   private Optional<String> organizationID;
   private Optional<String> firstName;
   private Optional<String> lastName;
   private Optional<String> email;
   
   public UserPatchRequest(String userID,
           Map<String, String> requestData) {
      this.userID = userID;
      for (Map.Entry<String, String> e : requestData.entrySet()) {
         switch (e.getKey()) {
            case ROLE_LABEL:
               this.role = Optional.of(Roles.fromString(e.getValue())); break;
            case EMAIL_LABEL:
               this.email = Optional.of(e.getValue()); break;
            case CONSORTIUM_ID_LABEL:
               this.consortiumID = Optional.of(e.getValue()); break;
            case ORGANIZATION_ID_LABEL:
               this.organizationID = Optional.of(e.getValue()); break;
            case FIRST_NAME_LABEL:
               this.firstName = Optional.of(e.getValue()); break;
            case LAST_NAME_LABEL:
               this.lastName = Optional.of(e.getValue()); break;
         }
      }
   }
   
   public String getUserID() {
      return userID;
   }
   
   public Optional<Roles> getRole() {
      return role;
   }

   public void setRole(Roles role) {
      this.role = Optional.of(role);
   }

   public Optional<String> getConsortiumID() {
      return consortiumID;
   }

   public void setConsortiumID(String consortiumID) {
      this.consortiumID = Optional.of(consortiumID);
   }

   public Optional<String> getOrganizationID() {
      return organizationID;
   }

   public void setOrganizationID(String organizationID) {
      this.organizationID = Optional.of(organizationID);
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
}
