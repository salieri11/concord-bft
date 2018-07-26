package profiles;

public class Organization {

   private String organizationID;
   private String organizationName;

   protected Organization(String organizationID, String organizationName) {
      this.organizationID = organizationID;
      this.organizationName = organizationName;
   }

   public String getOrganizationID() {
      return organizationID;
   }

   public void setOrganizationID(String organizationID) {
      this.organizationID = organizationID;
   }

   public String getOrganizationName() {
      return organizationName;
   }

   public void setOrganizationName(String organizationName) {
      this.organizationName = organizationName;
   }

}
