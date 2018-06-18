package database;


/**
 * <p>Copyright 2018 VMware, all rights reserved.</p>
 *
 */
public class ServiceUnavailableException extends Exception {
    private static final long serialVersionUID = 1L;
    
    public ServiceUnavailableException(String message) {
        super(message);
    }
}
