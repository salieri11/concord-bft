package contracts;

/**
 * <p> Copyright 2018 VMware, all rights reserved. </p>
 *
 * An interface for retrieving complete information about a particular contract.
 */
public interface FullVersionInfo extends BriefVersionInfo {
    String getByteCode();
    
    String getSourceCode();
}
