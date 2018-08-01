package profiles;

public enum Roles {

   ORG_USER("org_user"),
   ORG_DEVELOPER("org_developer"),
   ORG_ADMIN("org_admin"),
   CONSORTIUM_ADMIN("consortium_admin"),
   SYSTEM_ADMIN("system_admin");

   private final String name;

   Roles(String name) {
      this.name = name;
   }

   public static boolean contains(String s) {
      for (Roles r : Roles.values()) {
         if (r.toString().equals(s))
            return true;
      }
      return false;
   }

   public static Roles fromString(String s) {
      for (Roles r : Roles.values()) {
         if (r.toString().equals(s))
            return r;
      }
      return null;
   }

   public String toString() {
      return this.name;
   }
}
