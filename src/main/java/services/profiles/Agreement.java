/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 *
 */

package services.profiles;

import javax.persistence.*;

/**
 * A Spring Data JPA (or Hibernate) Entity class representing a user in the
 * system.
 */
@Table(name = "AGREEMENTS")
@Entity
public class Agreement {

   @Id
   @GeneratedValue(strategy = GenerationType.AUTO)
   private Long id;

   @Column(nullable = false)
   private Boolean accepted;

   @Column(nullable = false)
   private String type;

   @Column(nullable = false)
   private String content;


   public Long getID() {
      return id;
   }

   public String getContent() {
      return content;
   }

   public String getType() {
      return type;
   }

   public Boolean getAcceptance() {
      return accepted;
   }

   protected void accepted() {
      this.accepted = true;
   }

}
