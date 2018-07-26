package profiles;

import java.sql.SQLException;
import java.util.List;
import java.util.Optional;

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

   public List<User> getUserWithID(String userID) {
      return users.getUserWithID(userID);
   }

   public List<User> getUserWithEmail(String userEmail) {
      return users.getUserWithEmail(userEmail);
   }

   private boolean
           preCreationCheck(String email, String role, String consortiumName,
                            String organizationName) throws UserModificationException {
      // check if user is not already present
      // check if consortium and organization IDs are valid
      // check if role is valid
      boolean validRole = Roles.contains(role);
      if (!validRole)
         throw new UserModificationException("Provide role value is invalid.");
      List<User> existingUser = users.getUserWithEmail(email);
      if (existingUser.size() != 0)
         throw new UserModificationException("User with given email already "
            + "exists.");
      // TODO: Ideally the organization and consortium should already exist
      // before adding a USER to that. But for now we just add a new
      // organization & consortium
      List<Organization> existingOrg
         = organizations.getOrganizationByName(organizationName);
      try {
         if (existingOrg.size() != 1) {
            // throw new UserModificationException("No such organization
            // exists.");
            // for now don't throw - just create new
            organizations.addOrganization(organizationName);
         }
         List<Consortium> existingCon
            = consortiums.getConsortiumByName(consortiumName);
         if (existingCon.size() != 1) {
            // throw new UserModificationException("No such consortium
            // exists.");
            // for now don't throw - just create new
            // TODO: organization type is not specified in user creation POST
            // API
            // for now use "DEFAULT" as type, when we have separate POST API
            // for organization creation we will have proper types.
            consortiums.addConsortium(consortiumName, "DEFAULT");
         }
      } catch (SQLException e) {
         logger.warn("Unable to create organization or consortium", e);
         return false;
      }
      return true;
   }

   public void createUser(String name, String email, String role,
                          String consortiumID, String organizationID,
                          String password) throws UserModificationException {
      try {
         if (preCreationCheck(email, role, consortiumID, organizationID)) {
            User u = users.createUser(name,
                                      email,
                                      Optional.empty(),
                                      Optional.empty(),
                                      password);
            // Now add this user into roles table
            userRoles.insertRole(u.getUserID(),
                                 organizationID,
                                 consortiumID,
                                 Roles.valueOf(role));
         }
      } catch (SQLException e) {
         throw new UserModificationException(e.getMessage());
      }
   }

   public void createUser(String name, String email, String role,
                          String firstName, String lastName,
                          String consortiumID, String organizationID,
                          String password) throws UserModificationException {
      try {
         if (preCreationCheck(email, role, consortiumID, organizationID)) {
            User u = users.createUser(name,
                                      email,
                                      Optional.of(firstName),
                                      Optional.of(lastName),
                                      password);
            // Now add this user into roles table
            userRoles.insertRole(u.getUserID(),
                                 organizationID,
                                 consortiumID,
                                 Roles.valueOf(role));
         }
      } catch (SQLException e) {
         throw new UserModificationException(e.getMessage());
      }
   }

   // public User updateUser(UserPatchRequest upr) {
   //
   // }
}
