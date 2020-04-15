package tee;

import com.google.protobuf.ByteString;
import com.vmware.concord.tee.Tee.RawSkvbcRequest;
import com.vmware.concord.tee.Tee.RawSkvbcResponse;
import com.vmware.concord.tee.TeeServiceGrpc.TeeServiceBlockingStub;
import org.apache.logging.log4j.Logger;

import java.nio.ByteBuffer;

import static com.vmware.concord.tee.Tee.RawSkvbcRequest.newBuilder;
import static java.nio.ByteBuffer.allocate;
import static java.nio.ByteOrder.LITTLE_ENDIAN;
import static org.apache.logging.log4j.LogManager.getLogger;

/**
 * TEE skvbcWrite operation
 */
public class SkvbcWrite implements Operation {

    private static final Logger logger = getLogger(SkvbcWrite.class);

    /**
     * Structure of response payload:
     * <p>
     * opCode (1 byte) - possible values: read(1), write(2)
     * opStatus (1 byte) -  possible values: failure(0), success(1)
     * blockId (8 bytes)
     * </p>
     */
    private static final int RESPONSE_SIZE = 10;

    private final SkvbcPayload payload;
    private final TeeServiceBlockingStub blockingStub;

    public SkvbcWrite(int requestSize, TeeServiceBlockingStub blockingStub) {
        this.payload = new SkvbcPayload(requestSize);
        this.blockingStub = blockingStub;
    }

    @Override
    public void execute() {
        ByteString content = ByteString.copyFrom(payload.create());
        RawSkvbcRequest request = newBuilder().setContent(content).build();
        RawSkvbcResponse response = blockingStub.skvbcWrite(request);
        ByteBuffer buf = allocate(RESPONSE_SIZE).order(LITTLE_ENDIAN);
        response.getContent().copyTo(buf);
        buf.flip();

        logger.debug("OpCode: {}, OpStatus: {}, BlockId: {}", buf.get(), buf.get(), buf.getLong(2));
    }

}
