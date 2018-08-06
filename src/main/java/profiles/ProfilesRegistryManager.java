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
public class ProfilesRegistryManager {

   private static Logger logger
      = Logger.getLogger(ProfilesRegistryManager.class);

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

   public JSONArray getUsers(String consortiumID, String organizationID) {
      List<User> userList = getUsersInternal(consortiumID, organizationID);
      // @formatter:off
      return userList.stream()
              .map(UsersAPIMessage::new)
              .map(UsersGetResponse::toJSON)
              .reduce(new JSONArray(),
                      (arr, obj) -> { arr.add(obj); return arr; },
                      (arr1, arr2) -> { arr1.addAll(arr2); return arr1; });
      // @formatter:on
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

   public String createUser(UserCreateRequest request)
           throws UserModificationException {
      
      Optional<Organization> o
         = organizationRepository.findById(request.getOrganizationID());
      Optional<Consortium> c
         = consortiumRepository.findById(request.getConsortiumID());

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
         throw new UserModificationException(request.getRole() +
                 " is invalid Role value.");
      }
   }

   public void
          updateUser(UserPatchRequest request) throws UserModificationException {
      Optional<User> oUser
         = userRepository.findById(request.getUserID());

      if (!oUser.isPresent())
         throw new UserModificationException("No user found with ID: "
            + request.getUserID());

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
