package profiles;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import org.apache.log4j.Logger;

import database.ServiceUnavailableException;

public class UsersRegistryManager {
   private static UsersRegistryManager self = null;
   private static Object instanceLock = new Object();
   private static Logger logger = Logger.getLogger(UsersRegistryManager.class);

   private UsersDAO users;
   private OrganizationDAO organizations;
   private ConsortiumDAO consortiums;
   private UserRolesDAO userRoles;

   private UsersRegistryManager() throws ServiceUnavailableException,
                                  SQLException {
      users = new UsersDAO();
      organizations = new OrganizationDAO();
      consortiums = new ConsortiumDAO();
      userRoles = new UserRolesDAO();
   }

   /**
    * Returns an instance of UsersRegistryManager class. This method must be
    * thread safe as multiple client requests might try to read/write contract
    * registry at the same time and every request will call this method.
    *
    * @return
    */
   public static UsersRegistryManager getInstance() throws Exception {
      if (self == null) {
         // Make sure this is thread-safe
         synchronized (instanceLock) {
            if (self == null) {
               try {
                  self = new UsersRegistryManager();
               } catch (Exception e) {
                  logger.fatal("Error in initializing database.", e);
                  self = null;
                  throw new Exception("Error in initializing database."
                     + e.getMessage());
               }
            }
         }
      }
      return self;
   }

   public List<UserRole> getUsers(String consortiumID, String organizationID) {
      return userRoles.getUserRoles(Optional.ofNullable(consortiumID),
                                    Optional.ofNullable(organizationID));
   }
   
   public List<User> getUsersWithID(List<String> userIdList) {
      return userIdList.stream()
              .map(this::getUserWithID)
              .flatMap(List::stream)
              .collect(Collectors.toList());
   }

   public List<User> getUserWithID(String userID) {
      return users.getUserWithID(userID);
   }

   public List<User> getUserWithEmail(String userEmail) {
      return users.getUserWithEmail(userEmail);
   }

   private boolean
           preCreationCheck(String email, String role, String consortiumID,
                            String organizationID) throws UserModificationException {
      // check if user is not already present
      // check if consortium and organization IDs are valid
      // check if role is valid
      boolean validRole = Roles.contains(role);
      if (!validRole)
         throw new UserModificationException("Provided role value:" + role +
                 " is invalid.");
     
      List<User> existingUser = users.getUserWithEmail(email);
      if (existingUser.size() != 0)
         throw new UserModificationException("User with given email already "
            + "exists.");
      
      List<Organization> existingOrg
         = organizations.getOrganizationByID(organizationID);
      
      List<Consortium> existingCon
              = consortiums.getConsortiumByID(consortiumID);

      if (existingOrg.size() != 1) {
         throw new UserModificationException("No such organization exists.");
      }
      
      if (existingCon.size() != 1) {
         throw new UserModificationException("No such consortium exists.");
      }
      return true;
   }

   
   private String createUser(String name, String email, String role,
                             Optional<String> fName, Optional<String> lName,
                             String consortiumID, String organizationID,
                             String password) throws UserModificationException {
      try {
         if (preCreationCheck(email, role, consortiumID, organizationID)) {
            User newUser = users.createUser(name,
                    email,
                    fName, lName,
                    password);
            
            // Now add this user into roles table
            userRoles.insertRole(newUser.getUserID(),
                    organizationID,
                    consortiumID,
                    Roles.fromString(role));
            return newUser.getUserID();
         } else {
            throw new UserModificationException("User creation checks failed");
         }
      } catch (SQLException e) {
         throw new UserModificationException(e.getMessage());
      }
   }
   
   
   public String createUser(String name, String email, String role,
                          String consortiumID, String organizationID,
                          String password) throws UserModificationException {
      return createUser(name, email, role,
              Optional.empty(), Optional.empty(),
              consortiumID, organizationID, password);
   }

   public String createUser(String name, String email, String role,
                          String firstName, String lastName,
                          String consortiumID, String organizationID,
                          String password) throws UserModificationException {
      return createUser(name, email, role,
              Optional.of(firstName), Optional.of(lastName),
              consortiumID, organizationID, password);
   }
   
   
   public boolean updateUser(UserPatchRequest upr) throws SQLException {
      boolean result = true;
      if (upr.getFirstName().isPresent()) {
         result &=
                 users.updateFirstName(upr.getUserID(), upr.getFirstName().get());
      }
      if (upr.getLastName().isPresent()) {
         result &=
                 users.updateLastName(upr.getUserID(), upr.getLastName().get());
      }
      if (upr.getEmail().isPresent()) {
         result &=
                 users.updateEmail(upr.getUserID(), upr.getEmail().get());
      }
      if (upr.getRole().isPresent() &&
              upr.getConsortiumID().isPresent() &&
              upr.getOrganizationID().isPresent()) {
         result &=
                 userRoles.updateRole(upr.getUserID(),
                         upr.getConsortiumID().get(),
                         upr.getOrganizationID().get(), upr.getRole().get());
      }
      return result;
    }
   
   
   // TODO: This is just testing convenience methods and should be removed
   // when actual POST API for organization and consortium creation is
   // available
   public String createOrgIfNotExist(String orgName) {
      try {
         List<Organization> oList = organizations.getOrganizationByName(orgName);
         if (oList.isEmpty()) {
            return organizations.addOrganization(orgName);
         } else {
            return oList.get(0).getOrganizationID();
         }
      } catch (SQLException e) {
         logger.warn("Exception in org creation", e);
      }
      return null;
   }
   
   // TODO: This is just testing convenience methods and should be removed
   // when actual POST API for organization and consortium creation is
   // available
   public String createConsortiumIfNotExist(String consName, String
           consType) {
      try {
         List<Consortium> cList = consortiums.getConsortiumByName(consName);
         if (cList.isEmpty()) {
            return consortiums.addConsortium(consName, consType);
         } else {
            return cList.get(0).getConsortiumID();
         }
      } catch (SQLException e) {
         logger.warn("Exception in consortium creation", e);
      }
      return null;
   }
   
}
