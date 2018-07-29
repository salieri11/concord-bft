package profiles;

public class Organization {
   
   public static final String ORGANIZATION_LABEL = "organization";
   public static final String ORGANIZATION_ID_LABEL = "organization_id";
   public static final String ORGANIZATION_NAME_LABEL = "organization_name";
   
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
