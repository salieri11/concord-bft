// Copyright 2018 VMware, all rights reserved

#ifndef CONCORD_CONSENSUS_KVB_HEXTOOLS_H_
#define CONCORD_CONSENSUS_KVB_HEXTOOLS_H_

#include <stdio.h>
#include <string>

std::ostream &hexPrint(std::ostream &s, const uint8_t *data, size_t size);

#endif  // CONCORD_CONSENSUS_KVB_HEXTOOLS_H_
