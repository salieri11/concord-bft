package profiles;

import static profiles.Consortium.CONSORTIUM_LABEL;
import static profiles.Organization.ORGANIZATION_LABEL;

import java.time.Instant;

import javax.persistence.*;

import org.json.simple.JSONObject;
import org.springframework.lang.NonNull;

@Table(name = "USERS")
@Entity
public class User {

   public static final String USER_ID_LABEL = "user_id";
   public static final String NAME_LABEL = "name";
   public static final String FIRST_NAME_LABEL = "first_name";
   public static final String LAST_NAME_LABEL = "last_name";
   public static final String EMAIL_LABEL = "email";
   public static final String PASSWORD_LABEL = "password";
   public static final String ROLE_LABEL = "role";
   public static final String LAST_LOGIN_LABEL = "last_login";
   public static final String DETAILS_LABEL = "details";

   @Id
   @GeneratedValue(strategy = GenerationType.AUTO)
   private Long userID = 0L;

   @NonNull
   private String name;

   // firstName and lastName are primarily used for
   // internationalization purposes
   private String firstName;

   private String lastName;

   @NonNull
   private String email;

   @NonNull
   private String role;

   @ManyToOne(optional = false, fetch = FetchType.LAZY)
   @JoinColumn(nullable = false)
   private Organization organization;

   @ManyToOne(optional = false, fetch = FetchType.LAZY)
   @JoinColumn(nullable = false)
   private Consortium consortium;

   @NonNull

   private String password;

   private Instant lastLogin = Instant.MIN;

   public User() {
   }

   protected User(String name, String email, String firstName, String lastName,
                  Roles role, Organization organization, Consortium consortium,
                  String password) {
      this.name = name;
      this.email = email;
      this.firstName = firstName;
      this.lastName = lastName;
      this.role = role.toString();
      this.consortium = consortium;
      this.organization = organization;
      this.password = password;
   }

   protected User(String name, String email, Roles role,
                  Organization organization, Consortium consortium,
                  String password) {
      this(name, email, null, null, role, organization, consortium, password);
   }

   public Long getUserID() {
      return userID;
   }

   public String getName() {
      return name;
   }

   protected void setName(String name) {
      this.name = name;
   }

   public String getFirstName() {
      return firstName;
   }

   protected void setFirstName(String firstName) {
      this.firstName = firstName;
   }

   public String getLastName() {
      return lastName;
   }

   protected void setLastName(String lastName) {
      this.lastName = lastName;
   }

   public String getEmail() {
      return email;
   }

   protected void setEmail(String email) {
      this.email = email;
   }

   public String getRole() {
      return role;
   }

   protected void setRole(String role) {
      this.role = role;
   }

   public Organization getOrganization() {
      return organization;
   }

   public Consortium getConsortium() {
      return consortium;
   }

   public String getPassword() {
      return password;
   }

   protected void setPassword(String password) {
      this.password = password;
   }

   public Instant getLastLogin() {
      return lastLogin;
   }

   protected void setLastLogin(Instant lastLogin) {
      this.lastLogin = lastLogin;
   }

   public JSONObject toJSON() {
      JSONObject json = new JSONObject();
      json.put(NAME_LABEL, name);
      json.put(FIRST_NAME_LABEL, firstName);
      json.put(LAST_NAME_LABEL, lastName);
      json.put(EMAIL_LABEL, email);
      json.put(USER_ID_LABEL, userID);
      json.put(ROLE_LABEL, role);
      json.put(ORGANIZATION_LABEL, organization.toJSON());
      json.put(CONSORTIUM_LABEL, consortium.toJSON());
      json.put(LAST_LOGIN_LABEL, lastLogin);
      return json;
   }

   @Override
   public int hashCode() {
      return (int) ((userID * 53) % 17);
   }

   @Override
   public boolean equals(Object o) {
      if (o == null || !(o instanceof User))
         return false;
      User u = (User) o;
      return u.getUserID().equals(userID);
   }
}
