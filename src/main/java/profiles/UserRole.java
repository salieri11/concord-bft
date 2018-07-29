package profiles;

public class UserRole {
   
   public static final String ROLE_LABEL = "role";
   
   private String userID;
   private String organizationID;
   private String consortiumID;
   private Roles role;

   protected UserRole(String userID, String organizationID, String consortiumID,
                      Roles role) {
      this.userID = userID;
      this.consortiumID = consortiumID;
      this.organizationID = organizationID;
      this.role = role;
   }

   public String getUserID() {
      return userID;
   }

   public String getConsortiumID() {
      return consortiumID;
   }

   public String getOrganizationID() {
      return organizationID;
   }

   public Roles getRole() {
      return role;
   }
}
