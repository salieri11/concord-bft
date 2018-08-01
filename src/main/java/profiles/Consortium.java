package profiles;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.*;

import org.json.simple.JSONObject;

@Table(name = "CONSORTIUMS")
@Entity
public class Consortium {

   public static final String CONSORTIUM_LABEL = "consortium";
   public static final String CONSORTIUM_ID_LABEL = "consortium_id";
   public static final String CONSORTIUM_NAME_LABEL = "consortium_name";
   @OneToMany(mappedBy = "consortium",
              cascade = { CascadeType.PERSIST, CascadeType.REMOVE })
   protected Set<User> users = new HashSet<>();
   @Id
   @GeneratedValue(strategy = GenerationType.AUTO)
   private Long consortiumID = 0L;
   private String consortiumName;
   private String consortiumType;

   protected Consortium() {
   }

   public Consortium(String consortiumName, String consortiumType) {
      this.consortiumName = consortiumName;
      this.consortiumType = consortiumType;
   }

   public Long getConsortiumID() {
      return consortiumID;
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

   public Set<User> getUsers() {
      return Collections.unmodifiableSet(users);
   }

   public void addUser(User u) {
      users.add(u);
   }

   @Override
   public int hashCode() {
      return (int) ((consortiumID * 53) % 17);
   }

   @Override
   public boolean equals(Object o) {
      if (o == null || !(o instanceof Consortium))
         return false;
      Consortium c = (Consortium) o;
      return c.getConsortiumID().equals(consortiumID);
   }

   public JSONObject toJSON() {
      JSONObject json = new JSONObject();
      json.put(CONSORTIUM_ID_LABEL, consortiumID);
      json.put(CONSORTIUM_NAME_LABEL, consortiumName);
      return json;
   }
}
