/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 *
 */

package services.profiles;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.*;

/**
 * A Spring Data JPA (or Hibernate) Entity class representing a consortium in
 * the system.
 */
@Table(name = "CONSORTIUMS")
@Entity
public class Consortium {

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

   public Long getConsortiumID() {
      return consortiumID;
   }

   protected void setConsortiumID(Long consortiumID) {
      this.consortiumID = consortiumID;
   }

   public String getConsortiumName() {
      return consortiumName;
   }

   protected void setConsortiumName(String consortiumName) {
      this.consortiumName = consortiumName;
   }

   public String getConsortiumType() {
      return consortiumType;
   }

   protected void setConsortiumType(String consortiumType) {
      this.consortiumType = consortiumType;
   }

   public Set<User> getUsers() {
      return Collections.unmodifiableSet(users);
   }

   protected void addUser(User u) {
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
}
