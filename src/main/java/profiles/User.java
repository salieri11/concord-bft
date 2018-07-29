package profiles;

import org.json.simple.JSONObject;

public class User {
   
   public static final String USER_ID_LABEL = "user_id";
   public static final String NAME_LABEL = "name";
   public static final String FIRST_NAME_LABEL = "first_name";
   public static final String LAST_NAME_LABEL = "last_name";
   public static final String EMAIL_LABEL = "email";
   public static final String PASSWORD_LABEL = "password";
   
   
   private String userID;
   private String name;
   // firstName and lastName are primarily used for
   // internationalization purposes
   private String firstName;
   private String lastName;
   private String email;

   protected User(String userID, String name, String email) {
      this.userID = userID;
      this.name = name;
      this.email = email;
   }
   
   public User(String userID, String name, String email, String firstName,
               String lastName) {
      this.userID = userID;
      this.name = name;
      this.email = email;
      this.firstName = firstName;
      this.lastName = lastName;
   }
   
   public User(String name, String email, String firstName, String lastName) {
      this.name = name;
      this.email = email;
      this.firstName = firstName;
      this.lastName = lastName;
   }

   public String getUserID() {
      return userID;
   }

   public void setUserID(String userID) {
      this.userID = userID;
   }

   public String getName() {
      return name;
   }

   public void setName(String name) {
      this.name = name;
   }

   public String getFirstName() {
      return firstName;
   }

   public void setFirstName(String firstName) {
      this.firstName = firstName;
   }

   public String getLastName() {
      return lastName;
   }

   public void setLastName(String lastName) {
      this.lastName = lastName;
   }

   public String getEmail() {
      return email;
   }

   public void setEmail(String email) {
      this.email = email;
   }
   
   public JSONObject toJSON() {
      JSONObject json = new JSONObject();
      json.put(NAME_LABEL, name);
      json.put(FIRST_NAME_LABEL, firstName);
      json.put(LAST_NAME_LABEL, lastName);
      json.put(EMAIL_LABEL, email);
      json.put(USER_ID_LABEL, userID);
      return json;
   }
}
