package tee;

/**
 * A gRPC TEE operation
 */
@FunctionalInterface
public interface Operation {

    void execute();

    default long getBlockId() {
        return 0;
    }
}
