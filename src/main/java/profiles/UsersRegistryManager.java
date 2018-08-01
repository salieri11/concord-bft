package profiles;

import java.time.Instant;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import javax.transaction.Transactional;

import org.apache.log4j.Logger;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
@Transactional
public class UsersRegistryManager {

   private static Logger logger = Logger.getLogger(UsersRegistryManager.class);

   @Autowired
   private UserRepository userRepository;

   @Autowired
   private OrganizationRepository organizationRepository;

   @Autowired
   private ConsortiumRepository consortiumRepository;

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

   public JSONArray getUsers(String consortiumID, String organizationID) {
      List<User> userList = getUsersInternal(consortiumID, organizationID);
      return userList.stream().map(User::toJSON).reduce(new JSONArray(),
                                                        (arr, obj) -> {
                                                           arr.add(obj);
                                                           return arr;
                                                        },
                                                        (arr1, arr2) -> {
                                                           arr1.addAll(arr2);
                                                           return arr1;
                                                        });
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
      return oUser.map(User::toJSON).orElse(new JSONObject());
   }

   public String createUser(String name, String email, String role,
                            Optional<String> fName, Optional<String> lName,
                            String consortiumID, String organizationID,
                            String password) throws UserModificationException {
      Optional<Organization> o
         = organizationRepository.findById(Long.parseLong(organizationID));
      Optional<Consortium> c
         = consortiumRepository.findById(Long.parseLong(consortiumID));

      if (o.isPresent() && c.isPresent() && Roles.contains(role)) {
         User u;
         if (fName.isPresent() && lName.isPresent()) {
            u = new User(name,
                         email,
                         fName.get(),
                         lName.get(),
                         Roles.fromString(role),
                         o.get(),
                         c.get(),
                         password);
         } else {
            u = new User(name,
                         email,
                         Roles.fromString(role),
                         o.get(),
                         c.get(),
                         password);
         }
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
            + " ID " + organizationID + " not found."));
         c.orElseThrow(() -> new UserModificationException("Consortium with"
            + " ID " + consortiumID + " not found."));
         throw new UserModificationException(role + " is invalid Role value.");
      }
   }

   public void
          updateUser(UserPatchRequest upr) throws UserModificationException {
      Optional<User> oUser
         = userRepository.findById(Long.parseLong(upr.getUserID()));

      if (!oUser.isPresent())
         throw new UserModificationException("No user found with ID: "
            + upr.getUserID());

      User user = oUser.get();
      if (upr.getFirstName().isPresent()) {
         user.setFirstName(upr.getFirstName().get());
      }
      if (upr.getLastName().isPresent()) {
         user.setLastName(upr.getLastName().get());
      }
      if (upr.getEmail().isPresent()) {
         user.setEmail(upr.getEmail().get());
      }
      if (upr.getRole().isPresent()) {
         if (Roles.contains(upr.getRole().get())) {
            user.setRole(upr.getRole().get());
         } else {
            throw new UserModificationException("Invalid role value: "
               + upr.getRole().get());
         }
      }
      if (upr.getName().isPresent()) {
         user.setName(upr.getName().get());
      }

      userRepository.save(user);
   }

   public boolean loginUser(String userID,
                            String password) throws UserModificationException {
      Optional<User> oUser = userRepository.findById(Long.parseLong(userID));
      if (oUser.isPresent()) {
         User u = oUser.get();
         if (u.getPassword().equals(password)) {
            u.setLastLogin(Instant.now());
            userRepository.save(u);
            return true;
         } else {
            return false;
         }
      } else {
         throw new UserModificationException("No user found with ID: "
            + userID);
      }
   }

   // TODO: This is just testing convenience methods and should be removed
   // when actual POST API for organization and consortium creation is
   // available
   public Long createOrgIfNotExist() {
      List<Organization> oList = organizationRepository.findAll();
      if (oList.isEmpty()) {
         Organization o = new Organization("TEST_ORG");
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
         Consortium c = new Consortium("TEST_CON", "ATHENA");
         c = consortiumRepository.save(c);
         return c.getConsortiumID();
      } else {
         return cList.get(0).getConsortiumID();
      }
   }

}
