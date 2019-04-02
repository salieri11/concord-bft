// Concord
//
// Copyright (c) 2019 VMware, Inc. All Rights Reserved.
//
// This product is licensed to you under the Apache 2.0 license (the
// "License").  You may not use this product except in compliance with the
// Apache 2.0 License.
//
// This product may include a number of subcomponents with separate copyright
// notices and license terms. Your use of these subcomponents is subject to the
// terms and conditions of the subcomponent's license, as noted in the LICENSE
// file.
//

#ifndef REPLICA_STATE_SYNC_IMP_H
#define REPLICA_STATE_SYNC_IMP_H

#include "kvb/ReplicaStateSync.h"

namespace Blockchain {

class ReplicaStateSyncImp : public ReplicaStateSync {
 public:
  ~ReplicaStateSyncImp() override = default;

  uint64_t execute(log4cplus::Logger &logger, BlockchainDBAdapter &bcDBAdapter,
                   ILocalKeyValueStorageReadOnly &kvs,
                   BlockId lastReachableBlockId,
                   uint64_t lastExecutedSeqNum) override;
};

}  // namespace Blockchain

#endif
