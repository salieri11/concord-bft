package profiles;

public class User {

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
}
