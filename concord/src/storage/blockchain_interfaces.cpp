// Copyright (c) 2019 VMware, Inc. All Rights Reserved.

#include "blockchain_interfaces.h"

namespace concord {
namespace storage {

// Pure virtual destructors need to be defined within an abstract class if that
// very abstract class is used to delete a derived instanciation.
// See main.cpp for its usage.
ICommandsHandler::~ICommandsHandler() {}

}  // namespace storage
}  // namespace concord
