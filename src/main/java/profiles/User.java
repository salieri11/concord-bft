package profiles;

import java.time.Instant;
import javax.persistence.*;
import org.springframework.lang.NonNull;

@Table(name = "USERS")
@Entity
public class User {
   
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

   private Instant lastLogin = Instant.EPOCH;

   protected User() {
   }

   protected Long getUserID() {
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
   
   protected void setUserID(Long userID) {
      this.userID = userID;
   }
   
   protected void setOrganization(Organization organization) {
      this.organization = organization;
   }
   
   protected void setConsortium(Consortium consortium) {
      this.consortium = consortium;
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
