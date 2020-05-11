package tee;

/**
 * Type of TEE operation.
 */
public enum OperationType {

    RUN_TEST("RunTest"),

    SKVBC_WRITE("SkvbcWrite"),

    WRITE_BLOCK("WriteBlock");

    private final String id;

    OperationType(String id) {
        this.id = id;
    }

    /**
     * Get id.
     */
    public String getId() {
        return id;
    }

    /**
     * Get type from Id.
     */
    public static OperationType valueOfId(String id) {
        for (OperationType name : values()) {
            if (name.id.equalsIgnoreCase(id)) {
                return name;
            }
        }

        throw new IllegalArgumentException(id + " is not a TEE operation");
    }

}
