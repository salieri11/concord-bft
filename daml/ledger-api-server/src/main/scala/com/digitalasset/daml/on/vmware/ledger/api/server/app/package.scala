package com.digitalasset.daml.on.vmware.ledger.api.server

import com.daml.ledger.participant.state.v1.{ReadService, WriteService}

package object app {
  type ReadWriteService = ReadService with WriteService
}
