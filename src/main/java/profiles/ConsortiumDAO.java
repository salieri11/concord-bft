package profiles;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;

import database.ServiceUnavailableException;

public class ConsortiumDAO {

   static final String CONSORTIUMS_TABLE_NAME = "consortiums";
   static final String CONSORTIUM_NAME_COLUMN_LABEL = "consortium_name";
   static final String CONSORTIUM_ID_COLUMN_LABEL = "consortium_id";
   static final String CONSORTIUM_TYPE_COLUMN_LABEL = "consortium_type";
   // create table consortiums(consortium_id varchar primary key,
   // consortium_name varchar, consortium_type varchar)
   private static final String createConsortiumTableQuery = "CREATE TABLE IF"
      + " NOT EXISTS " + CONSORTIUMS_TABLE_NAME + "("
      + CONSORTIUM_NAME_COLUMN_LABEL + " " + "varchar, "
      + CONSORTIUM_TYPE_COLUMN_LABEL + " varchar, " + CONSORTIUM_ID_COLUMN_LABEL
      + " UUID PRIMARY KEY DEFAULT " + "gen_random_uuid())";
   private static Logger logger = Logger.getLogger(ConsortiumDAO.class);
   // select * from consortiums where cnosortium_id = ?
   private String getConsortiumByIDQuery
      = "SELECT * FROM " + CONSORTIUMS_TABLE_NAME + " WHERE "
         + CONSORTIUM_NAME_COLUMN_LABEL + " = ?";

   // select * from consortiums where consortium_name = ?
   private String getConsortiumByNameQuery
      = "SELECT * FROM " + CONSORTIUMS_TABLE_NAME + " WHERE "
         + CONSORTIUM_ID_COLUMN_LABEL + " = ?";

   // insert into consortiums values(?, ?, ?)
   private String addConsortiumQuery
      = "INSERT INTO " + CONSORTIUMS_TABLE_NAME + " values(?, ?)";

   private Connection con;
   private PreparedStatement getConsortiumByID;
   private PreparedStatement getConsortiumByName;
   private PreparedStatement insertConsortium;

   protected ConsortiumDAO() throws ServiceUnavailableException, SQLException {
      logger.debug(createConsortiumTableQuery);
      logger.debug(getConsortiumByIDQuery);
      logger.debug(getConsortiumByNameQuery);
      logger.debug(addConsortiumQuery);

      // con = DatabaseService.getDatabaseConnection();
      // con.createStatement().executeUpdate(createConsortiumTableQuery);
      // getConsortiumByID = con.prepareStatement(getConsortiumByIDQuery);
      // getConsortiumByName = con.prepareStatement(getConsortiumByNameQuery);
      // insertConsortium = con.prepareStatement(addConsortiumQuery);
   }

   public List<Consortium> getConsortiumByID(String consortiumID) {
      ResultSet rs;
      List<Consortium> cList = new ArrayList<>();
      try {
         rs = getConsortiumByID.executeQuery();
         while (rs.next()) {
            cList.add(new Consortium(rs.getString(CONSORTIUM_ID_COLUMN_LABEL),
                                     rs.getString(CONSORTIUM_NAME_COLUMN_LABEL),
                                     rs.getString(CONSORTIUM_TYPE_COLUMN_LABEL)));
         }
      } catch (SQLException e) {
         logger.warn("Exception while fetching consortium for ID: "
            + consortiumID, e);
      }
      return cList;
   }

   public List<Consortium> getConsortiumByName(String consortiumName) {
      ResultSet rs;
      List<Consortium> cList = new ArrayList<>();
      try {
         rs = getConsortiumByName.executeQuery();
         while (rs.next()) {
            cList.add(new Consortium(rs.getString(CONSORTIUM_ID_COLUMN_LABEL),
                                     rs.getString(CONSORTIUM_NAME_COLUMN_LABEL),
                                     rs.getString(CONSORTIUM_TYPE_COLUMN_LABEL)));
         }
      } catch (SQLException e) {
         logger.warn("Exception while fetching consortium for Name: "
            + consortiumName, e);
      }
      return cList;
   }

   public boolean addConsortium(String consortiumName,
                                String consortiumType) throws SQLException {
      insertConsortium.setString(1, consortiumName);
      insertConsortium.setString(2, consortiumType);
      return insertConsortium.execute();
   }
}
