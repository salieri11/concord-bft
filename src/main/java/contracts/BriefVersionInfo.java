package contracts;

/**
 * <p>
 * Copyright 2018 VMware, all rights reserved.
 * </p>
 *
 * An interface for retrieving brief information about a particular version of a
 * particular contract
 */
public interface BriefVersionInfo extends BriefContractInfo {
   String getAddress();

   String getMetaData();

   String getVersionName();
}
