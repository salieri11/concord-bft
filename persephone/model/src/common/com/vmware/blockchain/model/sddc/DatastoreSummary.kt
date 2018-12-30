/* **********************************************************************
 * Copyright 2018 VMware, Inc.  All rights reserved. VMware Confidential
 * *********************************************************************/
package com.vmware.blockchain.model.sddc

data class DatastoreSummary(
    val datastore: String,
    val name: String,
    val type: String,
    val free_space: Long,
    val capacity: Long
)
