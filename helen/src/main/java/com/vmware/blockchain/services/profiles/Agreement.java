/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 *
 */

package com.vmware.blockchain.services.profiles;

import java.time.Instant;
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
   @Column(unique = true)
   private Long id;

   @Column(nullable = false)
   private Boolean accepted;

   @Column(nullable = false)
   private String type;

   @Column(nullable = false)
   private String content;

   @Column(nullable = true)
   private String firstName;

   @Column(nullable = true)
   private String lastName;

   @Column(nullable = true)
   private String company;

   @Column(nullable = true)
   private Long acceptedOn;

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

   public String getFirstName() {
      return firstName;
   }

   public String getLastName() {
      return lastName;
   }

   public String getCompany() {
      return company;
   }

   public Long getAcceptedOn() {
      return acceptedOn;
   }

   protected void accepted() {
      this.accepted = true;
   }

   protected void setFirstName(String firstName) {
      this.firstName = firstName;
   }

   protected void setLastName(String lastName) {
      this.lastName = lastName;
   }

   protected void setCompany(String company) {
      this.company = company;
   }

   protected void setAcceptedOn() {
      this.acceptedOn = Instant.now().toEpochMilli();
   }

}
