/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 *
 */

package profiles;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.*;

/**
 * A Spring Data JPA (or Hibernate) Entity class representing an organization in
 * the system.
 */
@Table(name = "ORGANIZATIONS")
@Entity
public class Organization {
   @OneToMany(mappedBy = "organization",
              cascade = { CascadeType.PERSIST, CascadeType.REMOVE })
   protected Set<User> users = new HashSet<>();
   @Id
   @GeneratedValue(strategy = GenerationType.AUTO)
   private Long organizationID = 0L;
   private String organizationName;

   protected Organization() {
   }

   public Long getOrganizationID() {
      return organizationID;
   }

   protected void setOrganizationID(Long organizationID) {
      this.organizationID = organizationID;
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

}
