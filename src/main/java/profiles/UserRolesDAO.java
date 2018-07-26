package profiles;

import static profiles.ConsortiumDAO.CONSORTIUMS_TABLE_NAME;
import static profiles.ConsortiumDAO.CONSORTIUM_ID_COLUMN_LABEL;
import static profiles.OrganizationDAO.ORGANIZATIONS_TABLE_NAME;
import static profiles.OrganizationDAO.ORGANIZATION_ID_COLUMN_LABEL;
import static profiles.UsersDAO.USERS_TABLE_NAME;
import static profiles.UsersDAO.USER_ID_COLUMN_LABEL;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.apache.log4j.Logger;

import database.ServiceUnavailableException;

public class UserRolesDAO {

   static final String USER_ROLES_TABLE_NAME = "user_roles";
   static final String ROLE_COLUMN_LABEL = "role";
   // create table user_roles(user_id varchar, organization_id varchar,
   // consortium_id varchar, role varchar,
   // foreign key(user_id) references users(user_id),
   // foreign key(organization_id) references organizations(organization_id),
   // foreign key(consortium_id) references consortiums(consortium_id));
   private static final String createUserRolesTableQuery = "CREATE TABLE IF "
      + "NOT EXISTS " + USER_ROLES_TABLE_NAME + "(" + USER_ID_COLUMN_LABEL
      + " UUID, " + ORGANIZATION_ID_COLUMN_LABEL + " UUID, "
      + CONSORTIUM_ID_COLUMN_LABEL + " UUID, " + ROLE_COLUMN_LABEL
      + " varchar, " + "FOREIGN KEY(" + USER_ID_COLUMN_LABEL + ")"
      + " REFERENCES " + USERS_TABLE_NAME + "(" + USER_ID_COLUMN_LABEL + "), "
      + "FOREIGN KEY(" + ORGANIZATION_ID_COLUMN_LABEL + ") " + "REFERENCES "
      + ORGANIZATIONS_TABLE_NAME + "(" + ORGANIZATION_ID_COLUMN_LABEL + "), "
      + "FOREIGN KEY(" + CONSORTIUM_ID_COLUMN_LABEL + ") REFERENCES "
      + CONSORTIUMS_TABLE_NAME + "(" + CONSORTIUM_ID_COLUMN_LABEL + "))";
   // select * from user_roles where user_id = "?"
   private static final String getUserRoleQuery = "SELECT * FROM "
      + USER_ROLES_TABLE_NAME + " WHERE " + USER_ID_COLUMN_LABEL + " = ?";
   // select * from user_roles where organization_id = "?"
   private static final String getOrganizationRolesQuery
      = "SELECT * FROM " + USER_ROLES_TABLE_NAME + " WHERE "
         + ORGANIZATION_ID_COLUMN_LABEL + " = ?";
   // select * from user_roles where consortium_id = "?"
   private static final String getConsortiumRolesQuery = "SELECT * FROM "
      + USER_ROLES_TABLE_NAME + " WHERE " + CONSORTIUM_ID_COLUMN_LABEL + " = ?";
   // select * from user_roles where consortium_id = "?" and
   // organization_id = "?"
   private static final String getOrganizationConsortiumRolesQuery
      = "SELECT * FROM " + USER_ROLES_TABLE_NAME + " WHERE "
         + ORGANIZATION_ID_COLUMN_LABEL + " = ? AND "
         + CONSORTIUM_ID_COLUMN_LABEL + " = ?";
   // insert into roles values(?, ?, ?, ?)
   private static final String insertRoleQuery
      = "INSERT INTO " + USER_ROLES_TABLE_NAME + " values(?, ?, ?, ?)";
   private static Logger logger = Logger.getLogger(UserRolesDAO.class);
   private Connection con;
   private PreparedStatement getUserRole;
   private PreparedStatement getOrganizationRoles;
   private PreparedStatement getConsortiumRoles;
   private PreparedStatement getOrganizationConsortiumRoles;
   private PreparedStatement insertRole;

   protected UserRolesDAO() throws ServiceUnavailableException, SQLException {
      logger.debug(createUserRolesTableQuery);
      logger.debug(getUserRoleQuery);
      logger.debug(getOrganizationRolesQuery);
      logger.debug(getConsortiumRolesQuery);
      logger.debug(getOrganizationConsortiumRolesQuery);
      logger.debug(insertRoleQuery);

      // con = DatabaseService.getDatabaseConnection();
      // con.createStatement().executeUpdate(createUserRolesTableQuery);
      // getUserRole = con.prepareStatement(getUserRoleQuery);
      // getOrganizationRoles = con.prepareStatement(getOrganizationRolesQuery);
      // getConsortiumRoles = con.prepareStatement(getConsortiumRolesQuery);
      // getOrganizationConsortiumRoles =
      // con.prepareStatement(getOrganizationConsortiumRolesQuery);
      // insertRole = con.prepareStatement(insertRoleQuery);
   }

   public List<UserRole> getUserRole(String userID) throws SQLException {
      List<UserRole> roleList = new ArrayList<>();
      ResultSet rs = getUserRole.executeQuery();
      while (rs.next()) {
         roleList.add(new UserRole(rs.getString(USER_ID_COLUMN_LABEL),
                                   rs.getString(ORGANIZATION_ID_COLUMN_LABEL),
                                   rs.getString(CONSORTIUM_ID_COLUMN_LABEL),
                                   Roles.valueOf(rs.getString(ROLE_COLUMN_LABEL))));
      }
      return roleList;
   }

   public List<UserRole> getUserRoles(Optional<String> consortiumID,
                                      Optional<String> organizationID) {
      List<UserRole> roleList = new ArrayList<>();
      ResultSet rs = null;
      try {
         if (consortiumID.isPresent() && organizationID.isPresent()) {
            rs = getOrganizationConsortiumRoles.executeQuery();
         } else if (consortiumID.isPresent()) {
            rs = getConsortiumRoles.executeQuery();
         } else if (organizationID.isPresent()) {
            rs = getOrganizationRoles.executeQuery();
         }

         while (rs != null && rs.next()) {
            roleList.add(new UserRole(rs.getString(USER_ID_COLUMN_LABEL),
                                      rs.getString(ORGANIZATION_ID_COLUMN_LABEL),
                                      rs.getString(CONSORTIUM_ID_COLUMN_LABEL),
                                      Roles.valueOf(rs.getString(ROLE_COLUMN_LABEL))));
         }

      } catch (SQLException e) {
         logger.warn("Exception in getting user roles", e);
      }

      return roleList;
   }

   public boolean insertRole(String userID, String organizationID,
                             String consortiumID, Roles role) {
      try {
         insertRole.setString(1, userID);
         insertRole.setString(2, consortiumID);
         insertRole.setString(3, organizationID);
         insertRole.setString(4, role.name());
         return insertRole.execute();
      } catch (SQLException e) {
         logger.warn("Exception when inserting new role: ", e);
         return false;
      }
   }
}
