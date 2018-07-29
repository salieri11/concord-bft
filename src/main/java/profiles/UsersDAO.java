package profiles;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import database.DatabaseService;
import org.apache.log4j.Logger;

import database.ServiceUnavailableException;

public class UsersDAO {

   static final String USERS_TABLE_NAME = "users";
   static final String USER_ID_COLUMN_LABEL = "user_id";
   static final String NAME_COLUMN_LABEL = "name";
   static final String FIRST_NAME_COLUMN_LABEL = "first_name";
   static final String LAST_NAME_COLUMN_LABEL = "last_name";
   static final String EMAIL_COLUMN_LABEL = "email";
   static final String PASSWORD_COLUMN_LABEL = "password";
   // create table users(first_name varchar, last_name varchar, email varchar,
   // user_id varchar primary key, password varchar);
   private static final String createUsersTableQuery = "CREATE "
      + "TABLE IF NOT EXISTS " + USERS_TABLE_NAME + "(" + NAME_COLUMN_LABEL
      + " varchar, " + FIRST_NAME_COLUMN_LABEL + " varchar, "
      + LAST_NAME_COLUMN_LABEL + " varchar, " + EMAIL_COLUMN_LABEL
      + " varchar, " + PASSWORD_COLUMN_LABEL + " varchar, "
      + USER_ID_COLUMN_LABEL + " UUID PRIMARY KEY DEFAULT gen_random_uuid())";
   private static Logger logger = Logger.getLogger(UsersDAO.class);
   private Connection con;

   // select * form users where user_id = ?
   private String getUserWithIDQuery = "SELECT * FROM " + USERS_TABLE_NAME
      + " WHERE " + USER_ID_COLUMN_LABEL + " = ?";

   // select * from users where email = ?
   private String getUserWithEmailQuery = "SELECT * FROM " + USERS_TABLE_NAME
      + " WHERE " + EMAIL_COLUMN_LABEL + " = ?";

   // insert into users values(?, ?, ?, ?, ?)
   private String insertUserQuery
      = "INSERT INTO " + USERS_TABLE_NAME + " values(?, ?, ?, ?, ?)";
   
   
   // update users set email = ? where user_id = ?
   private String updateEmailQuery = "UPDATE " + USERS_TABLE_NAME + " set " +
           EMAIL_COLUMN_LABEL + " = ? WHERE " + USER_ID_COLUMN_LABEL + " = ?";
   
   private String updateFirstNameQuery = "UPDATE " + USERS_TABLE_NAME + " set" +
           " " +
           FIRST_NAME_COLUMN_LABEL + " = ? WHERE " + USER_ID_COLUMN_LABEL + "" +
           " = ?";
   
   private String updateLastNameQuery = "UPDATE " + USERS_TABLE_NAME + " set" +
           " " +
           LAST_NAME_COLUMN_LABEL + " = ? WHERE " + USER_ID_COLUMN_LABEL + "" +
           " = ?";
   
   private String updateNameQuery = "UPDATE " + USERS_TABLE_NAME + " set" +
           " " +
           NAME_COLUMN_LABEL + " = ? WHERE " + USER_ID_COLUMN_LABEL + "" +
           " = ?";

   private PreparedStatement getUserWithIDPstmt;
   private PreparedStatement getUserWithEmailPstmt;
   private PreparedStatement insertUserPstmt;
   private PreparedStatement updateEmail;
   private PreparedStatement updateFirstName;
   private PreparedStatement updateLastName;
   private PreparedStatement updateName;
   
   protected UsersDAO() throws ServiceUnavailableException, SQLException {

      logger.debug(createUsersTableQuery);
      logger.debug(getUserWithEmailQuery);
      logger.debug(getUserWithIDQuery);
      logger.debug(insertUserQuery);
      logger.debug(updateEmailQuery);
      logger.debug(updateFirstNameQuery);
      logger.debug(updateLastNameQuery);
      logger.debug(updateNameQuery);

       con = DatabaseService.getDatabaseConnection();
       con.createStatement().executeUpdate(createUsersTableQuery);

       getUserWithIDPstmt = con.prepareStatement(getUserWithIDQuery);
       getUserWithEmailPstmt = con.prepareStatement(getUserWithEmailQuery);
       insertUserPstmt = con.prepareStatement(insertUserQuery);
       updateEmail = con.prepareStatement(updateEmailQuery);
       updateFirstName = con.prepareStatement(updateFirstNameQuery);
       updateLastName = con.prepareStatement(updateLastNameQuery);
       updateName = con.prepareStatement(updateNameQuery);
   }

   public List<User> getUserWithID(String userID) {
      List<User> userList = new ArrayList<>();
      try {
         getUserWithIDPstmt.setString(1, userID);
         ResultSet rs = getUserWithIDPstmt.executeQuery();
         while (rs.next()) {
            userList.add(new User(
                    rs.getString(USER_ID_COLUMN_LABEL),
                    rs.getString(NAME_COLUMN_LABEL),
                    rs.getString(EMAIL_COLUMN_LABEL),
                    rs.getString(FIRST_NAME_COLUMN_LABEL),
                    rs.getString(LAST_NAME_COLUMN_LABEL)));
         }
      } catch (SQLException e) {
         logger.warn("Exception with get user from ID query", e);
      }
      return userList;
   }

   public List<User> getUserWithEmail(String email) {
      List<User> userList = new ArrayList<>();
      try {
         getUserWithEmailPstmt.setString(1, email);
         ResultSet rs = getUserWithEmailPstmt.executeQuery();
         while (rs.next()) {
            userList.add(new User(
                    rs.getString(USER_ID_COLUMN_LABEL),
                    rs.getString(NAME_COLUMN_LABEL),
                    rs.getString(EMAIL_COLUMN_LABEL),
                    rs.getString(FIRST_NAME_COLUMN_LABEL),
                    rs.getString(LAST_NAME_COLUMN_LABEL)));
         }
      } catch (SQLException e) {
         logger.warn("Exception with get user from email query", e);
      }
      return userList;
   }

   public User createUser(String name, String email, Optional<String> firstName,
                          Optional<String> lastName,
                          String password) throws SQLException {
      String fName = firstName.isPresent() ? firstName.get() : "";
      String lName = lastName.isPresent() ? lastName.get() : "";
      insertUserPstmt.setString(1, name);
      insertUserPstmt.setString(2, fName);
      insertUserPstmt.setString(3, lName);
      insertUserPstmt.setString(4, email);
      insertUserPstmt.setString(5, password);
      insertUserPstmt.execute();

      List<User> userList = getUserWithEmail(email);
      return userList.get(0);
   }

   public boolean updateEmail(String userID, String newEmail) throws SQLException {
      updateEmail.setString(1, newEmail);
      updateEmail.setString(2, userID);
      return updateEmail.execute();
   }
   
   public boolean updateFirstName(String userID, String firstName) throws SQLException {
      updateFirstName.setString(1, firstName);
      updateFirstName.setString(2, userID);
      return updateFirstName.execute();
   }
   
   public boolean updateLastName(String userID, String lastName) throws
           SQLException  {
      updateLastName.setString(1, lastName);
      updateLastName.setString(2, userID);
      return updateLastName.execute();
   }
   
   public boolean updateName(String userID, String name) throws SQLException {
      updateName.setString(1, name);
      updateName.setString(2, userID);
      return updateName.execute();
   }
   
}
