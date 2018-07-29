package profiles;

public class Consortium {
   
   public static final String CONSORTIUM_LABEL = "consortium";
   public static final String CONSORTIUM_ID_LABEL = "consortium_id";
   public static final String CONSORTIUM_NAME_LABEL = "consortium_name";
   
   private String consortiumID;
   private String consortiumName;
   private String consortiumType;

   protected Consortium(String consortiumID, String consortiumName,
                        String consortiumType) {
      this.consortiumID = consortiumID;
      this.consortiumName = consortiumName;
      this.consortiumType = consortiumType;
   }

   public String getConsortiumID() {
      return consortiumID;
   }

   public void setConsortiumID(String consortiumID) {
      this.consortiumID = consortiumID;
   }

   public String getConsortiumName() {
      return consortiumName;
   }

   public void setConsortiumName(String consortiumName) {
      this.consortiumName = consortiumName;
   }

   public String getConsortiumType() {
      return consortiumType;
   }

   public void setConsortiumType(String consortiumType) {
      this.consortiumType = consortiumType;
   }
}
