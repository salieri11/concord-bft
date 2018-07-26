package profiles;

import java.util.Optional;

public class UserPatchRequest {
   private Optional<Roles> role;
   private Optional<String> consortiumID;
   private Optional<String> organizationID;
   private Optional<String> firstName;
   private Optional<String> lastName;
   private Optional<String> email;

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
