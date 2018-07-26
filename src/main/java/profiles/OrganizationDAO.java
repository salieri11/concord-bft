package profiles;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;

import database.ServiceUnavailableException;

public class OrganizationDAO {

   static final String ORGANIZATIONS_TABLE_NAME = "organizations";
   static final String ORGANIZATION_ID_COLUMN_LABEL = "organization_id";
   static final String ORGANIZATION_NAME_COLUMN_LABEL = "organization_name";
   // create table organizations(organization_id varchar primary key,
   // organization_name varchar)
   private static final String createOrgTableQuery
      = "CREATE TABLE IF NOT " + "EXISTS " + ORGANIZATIONS_TABLE_NAME + "("
         + ORGANIZATION_NAME_COLUMN_LABEL + " varchar, "
         + ORGANIZATION_ID_COLUMN_LABEL + " UUID PRIMARY KEY DEFAULT "
         + "gen_random_uuid())";
   private static Logger logger = Logger.getLogger(OrganizationDAO.class);
   // select * from organizations where organization_id = ?
   private String getOrganizationByIDQuery
      = "SELECT * FROM " + ORGANIZATIONS_TABLE_NAME + " WHERE "
         + ORGANIZATION_ID_COLUMN_LABEL + " = ?";

   // select * from organizations where organization_name = ?
   private String getOrganizationByNameQuery
      = "SELECT * FROM " + ORGANIZATIONS_TABLE_NAME + " WHERE "
         + ORGANIZATION_NAME_COLUMN_LABEL + " = ?";

   // insert into organizations values(?)
   private String insertOrganizationQuery
      = "INSERT INTO " + ORGANIZATIONS_TABLE_NAME + " values(?)";

   private Connection con;
   private PreparedStatement getOrganizationByID;
   private PreparedStatement getOrganizationByName;
   private PreparedStatement insertOrganization;

   protected OrganizationDAO() throws ServiceUnavailableException,
                               SQLException {
      logger.debug(createOrgTableQuery);
      logger.debug(getOrganizationByIDQuery);
      logger.debug(getOrganizationByNameQuery);
      logger.debug(insertOrganizationQuery);

      // con = DatabaseService.getDatabaseConnection();
      // con.createStatement().executeUpdate(createOrgTableQuery);
      // getOrganizationByID = con.prepareStatement(getOrganizationByIDQuery);
      // getOrganizationByName =
      // con.prepareStatement(getOrganizationByNameQuery);
      // insertOrganization = con.prepareStatement(insertOrganizationQuery);
   }

   public List<Organization> getOrganizationByID(String organizationID) {
      ResultSet rs;
      List<Organization> oList = new ArrayList<>();
      try {
         rs = getOrganizationByID.executeQuery();
         while (rs.next()) {
            oList.add(new Organization(rs.getString(ORGANIZATION_ID_COLUMN_LABEL),
                                       rs.getString(ORGANIZATION_NAME_COLUMN_LABEL)));
         }
      } catch (SQLException e) {
         logger.warn("Exception fetching organization with ID: "
            + organizationID);
      }
      return oList;
   }

   public List<Organization> getOrganizationByName(String organizationName) {
      ResultSet rs;
      List<Organization> oList = new ArrayList<>();
      try {
         rs = getOrganizationByName.executeQuery();
         while (rs.next()) {
            oList.add(new Organization(rs.getString(ORGANIZATION_ID_COLUMN_LABEL),
                                       rs.getString(ORGANIZATION_NAME_COLUMN_LABEL)));
         }
      } catch (SQLException e) {
         logger.warn("Exception fetching organization with Name: "
            + organizationName);
      }
      return oList;
   }

   public boolean addOrganization(String organizationName) throws SQLException {
      insertOrganization.setString(1, organizationName);
      return insertOrganization.execute();
   }
}
