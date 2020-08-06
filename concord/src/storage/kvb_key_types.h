// Copyright 2019 VMware, all rights reserved

#ifndef CONCORD_STORAGE_KVB_KEY_TYPES_H_
#define CONCORD_STORAGE_KVB_KEY_TYPES_H_

namespace concord {
namespace storage {

// Eth 0x00 - 0x0f
const char kKvbKeyEthBlock = 0x01;
const char kKvbKeyEthTransaction = 0x02;
const char kKvbKeyEthBalance = 0x03;
const char kKvbKeyEthCode = 0x04;
const char kKvbKeyEthStorage = 0x05;
const char kKvbKeyEthNonce = 0x06;

// HLF 0x10 - 0x1f
const char kKvbKeyHlfBlock = 0x10;
const char kKvbKeyHlfTransaction = 0x11;
const char kKvbKeyHlfState = 0x12;

// Concord 0x20 - 0x2f
const char kKvbKeyTimeSamples = 0x20;
const char kKvbKeyMetadata = 0x21;
const char kKvbKeySummarizedTime = 0x22;
const char kKvbKeyCorrelationId = 0x23;
const char kKvbKeyLastAgreedPrunableBlockId = 0x24;

// DAML 0x30 - 0x3f
const char kKvbKeyDaml = 0x30;
const char kKvbKeyAdminIdentifier = 0x40;

}  // namespace storage
}  // namespace concord

#endif  // CONCORD_STORAGE_KVB_KEY_TYPES_H_
