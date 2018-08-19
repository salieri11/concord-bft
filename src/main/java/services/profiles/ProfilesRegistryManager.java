/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 *
 */

package services.profiles;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import javax.transaction.Transactional;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * This class manages all persistence related operations related to User
 * management API.
 */
@Component
@Transactional
public class ProfilesRegistryManager {

   private static Logger logger
      = LogManager.getLogger(ProfilesRegistryManager.class);

   @Autowired
   private UserRepository userRepository;

   @Autowired
   private OrganizationRepository organizationRepository;

   @Autowired
   private ConsortiumRepository consortiumRepository;

   /** Needed for spring */
   protected ProfilesRegistryManager() {
   }

   private List<User> getUsersInternal(String consortiumID,
                                       String organizationID) {
      Optional<Organization> org
         = organizationRepository.findById(Long.parseLong(organizationID));
      Optional<Consortium> con
         = consortiumRepository.findById(Long.parseLong(consortiumID));

      if (org.isPresent() && con.isPresent()) {
         return userRepository.findUsersByConsortiumAndOrganization(con.get(),
                                                                    org.get());
      } else {
         return Collections.emptyList();
      }
   }

   private List<User> getUsersInternalByConsortiumID(String consortiumID) {
      Optional<Consortium> con
         = consortiumRepository.findById(Long.parseLong(consortiumID));
      if (con.isPresent()) {
         return new ArrayList<>(con.get().getUsers());
      } else {
         return Collections.emptyList();
      }
   }

   private List<User> getUsersInternalByOrganizationID(String organizationID) {
      Optional<Organization> org
         = organizationRepository.findById(Long.parseLong(organizationID));
      if (org.isPresent()) {
         return new ArrayList<>(org.get().getUsers());
      } else {
         return Collections.emptyList();
      }
   }

   public JSONArray getUsers(Optional<String> consortiumID,
                             Optional<String> organizationID) {
      List<User> userList;
      if (consortiumID.isPresent() && organizationID.isPresent()) {
         userList = getUsersInternal(consortiumID.get(), organizationID.get());
      } else if (consortiumID.isPresent()) {
         userList = getUsersInternalByConsortiumID(consortiumID.get());
      } else if (organizationID.isPresent()) {
         userList = getUsersInternalByOrganizationID(organizationID.get());
      } else {
         userList = userRepository.findAll();
      }

      return userList.stream()
              .map(UsersAPIMessage::new)
              .map(UsersGetResponse::toJSON)
              .reduce(new JSONArray(),
                      (arr, obj) -> { arr.add(obj); return arr; },
                      (arr1, arr2) -> { arr1.addAll(arr2); return arr1;});
   }

   private List<User> getUsersWithID(List<String> userIdList) {
      return userIdList.stream()
                       .map(this::getUserWithIDInternal)
                       .filter(Optional::isPresent)
                       .map(Optional::get)
                       .collect(Collectors.toList());
   }

   private Optional<User> getUserWithIDInternal(String userID) {
      return userRepository.findById(Long.parseLong(userID));
   }

   public JSONObject getUserWithID(String userID) {
      Optional<User> oUser = getUserWithIDInternal(userID);
      return oUser.map(UsersAPIMessage::new)
                  .map(UsersAPIMessage::toJSON)
                  .orElse(new JSONObject());
   }

   private boolean isDuplicateEmail(String email) {
      Optional<User> existingUserWithSameEmail
         = userRepository.findUserByEmail(email);
      return existingUserWithSameEmail.isPresent();
   }

   public String
          createUser(UserCreateRequest request) throws UserModificationException {

      Optional<Organization> o
         = organizationRepository.findById(request.getOrganizationID());
      Optional<Consortium> c
         = consortiumRepository.findById(request.getConsortiumID());

      // First check if user with same email already exists
      if (isDuplicateEmail(request.getEmail())) {
         throw new UserModificationException("Duplicate email address");
      }

      if (o.isPresent() && c.isPresent() && Roles.contains(request.getRole())) {
         User u = new User();
         u.setName(request.getUserName());
         u.setEmail(request.getEmail());
         u.setPassword(request.getPassword());
         u.setRole(request.getRole());
         u.setOrganization(o.get());
         u.setConsortium(c.get());
         request.getOptionalFirstName().ifPresent(u::setFirstName);
         request.getOptionalLastName().ifPresent(u::setLastName);
         // Note: The order of next 5 statements is very important, The user
         // object must be saved before it can be added and saved into
         // consortium & organization objects.
         u = userRepository.save(u);
         o.get().addUser(u);
         c.get().addUser(u);
         consortiumRepository.save(c.get());
         organizationRepository.save(o.get());
         return Long.toString(u.getUserID());
      } else {
         o.orElseThrow(() -> new UserModificationException("Organization with"
            + " ID " + request.getOrganizationID() + " not found."));
         c.orElseThrow(() -> new UserModificationException("Consortium with"
            + " ID " + request.getConsortiumID() + " not found."));
         throw new UserModificationException(request.getRole()
            + " is invalid Role value.");
      }
   }

   public void
          updateUser(UserPatchRequest request) throws UserModificationException {
      Optional<User> oUser = userRepository.findById(request.getUserID());

      if (!oUser.isPresent())
         throw new UserModificationException("No user found with ID: "
            + request.getUserID());

      // First check if user with same email already exists
      if (request.getOptionalEmail().isPresent()
         && isDuplicateEmail(request.getOptionalEmail().get())) {
         throw new UserModificationException("Duplicate email address");
      }

      User user = oUser.get();
      request.getOptionalName().ifPresent(user::setName);
      request.getOptionalEmail().ifPresent(user::setEmail);
      request.getOptionalFirstName().ifPresent(user::setFirstName);
      request.getOptionalLastName().ifPresent(user::setLastName);
      if (request.getOptionalRole().isPresent()) {
         if (Roles.contains(request.getOptionalRole().get())) {
            user.setRole(request.getOptionalRole().get());
         } else {
            throw new UserModificationException("Invalid role value: "
               + request.getOptionalRole().get());
         }
      }
      userRepository.save(user);
   }

   public boolean loginUser(String email,
                            String password) throws UserModificationException {
      Optional<User> oUser = userRepository.findUserByEmail(email);
      if (oUser.isPresent()) {
         User u = oUser.get();
         // TODO: We know this is not a long-term solution and this will be
         // replaced by CSP authentication very soon.
         if (password != null && u.getPassword().equals(password)) {
            u.setLastLogin(Instant.now().toEpochMilli());
            userRepository.save(u);
            return true;
         } else {
            return false;
         }
      } else {
         throw new UserModificationException("No user found with email: "
            + email);
      }
   }

   // TODO: This is just testing convenience methods and should be removed
   // when actual POST API for organization and consortium creation is
   // available
   public Long createOrgIfNotExist() {
      List<Organization> oList = organizationRepository.findAll();
      if (oList.isEmpty()) {
         Organization o = new Organization();
         o.setOrganizationName("TEST_ORG");
         o = organizationRepository.save(o);
         return o.getOrganizationID();
      } else {
         return oList.get(0).getOrganizationID();
      }
   }

   // TODO: This is just testing convenience methods and should be removed
   // when actual POST API for organization and consortium creation is
   // available
   public Long createConsortiumIfNotExist() {
      List<Consortium> cList = consortiumRepository.findAll();
      if (cList.isEmpty()) {
         Consortium c = new Consortium();
         c.setConsortiumName("TEST_CON");
         c.setConsortiumType("ATHENA");
         c = consortiumRepository.save(c);
         return c.getConsortiumID();
      } else {
         return cList.get(0).getConsortiumID();
      }
   }

}
