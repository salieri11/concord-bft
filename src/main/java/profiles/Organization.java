package profiles;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.*;

import org.json.simple.JSONObject;

@Table(name = "ORGANIZATIONS")
@Entity
public class Organization {

   public static final String ORGANIZATION_LABEL = "organization";
   public static final String ORGANIZATION_ID_LABEL = "organization_id";
   public static final String ORGANIZATION_NAME_LABEL = "organization_name";
   @OneToMany(mappedBy = "organization",
              cascade = { CascadeType.PERSIST, CascadeType.REMOVE })
   protected Set<User> users = new HashSet<>();
   @Id
   @GeneratedValue(strategy = GenerationType.AUTO)
   private Long organizationID = 0L;
   private String organizationName;

   protected Organization() {
   }

   protected Organization(String organizationName) {
      this.organizationName = organizationName;
   }

   public Long getOrganizationID() {
      return organizationID;
   }

   public String getOrganizationName() {
      return organizationName;
   }

   protected void setOrganizationName(String organizationName) {
      this.organizationName = organizationName;
   }

   public Set<User> getUsers() {
      return Collections.unmodifiableSet(users);
   }

   protected void addUser(User u) {
      users.add(u);
   }

   @Override
   public int hashCode() {
      return (int) ((organizationID * 53) % 17);
   }

   @Override
   public boolean equals(Object o) {
      if (o == null || !(o instanceof Organization))
         return false;
      Organization org = (Organization) o;
      return org.getOrganizationID().equals(organizationID);
   }

   public JSONObject toJSON() {
      JSONObject json = new JSONObject();
      json.put(ORGANIZATION_ID_LABEL, organizationID);
      json.put(ORGANIZATION_NAME_LABEL, organizationName);
      return json;
   }
}
