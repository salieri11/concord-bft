// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.execution.engine

import com.daml.ledger.validator.privacy.{
  AccessControlList,
  LedgerStateOperationsWithAccessControl,
  PublicAccess,
  RestrictedAccess
}
import com.digitalasset.daml.on.vmware.common.Conversions.toReplicaId
import com.digitalasset.daml.on.vmware.execution.engine.Digests.{
  hexDigestOfBytes,
  hexDigestOfStrings
}
import com.digitalasset.daml.on.vmware.execution.engine.metrics.ConcordLedgerStateOperationsMetrics
import com.digitalasset.kvbc.daml_validator.EventFromValidator.Read
import com.digitalasset.kvbc.daml_validator.{
  EventFromValidator,
  EventToValidator,
  KeyValuePair,
  ProtectedKeyValuePair
}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, Promise}

final class ConcordLedgerStateOperations(
    sendEvent: EventFromValidator => Unit,
    metrics: ConcordLedgerStateOperationsMetrics)(implicit val executionContext: ExecutionContext)
    extends LedgerStateOperationsWithAccessControl
    with ConcordLedgerStateReadHandler[EventToValidator.ReadResult, KeyValuePair] {
  import ConcordLedgerStateOperations.accessControlListToThinReplicaIds

  private[engine] val pendingWrites = mutable.Buffer[(Key, Value, AccessControlList)]()

  override def read(keys: Seq[Key]): Future[Seq[Option[Value]]] = {
    // Create a promise that we'll fulfill from [[handleReadResult]] when the
    // result arrives.
    val promise = Promise[Seq[KeyValuePair]]()
    val readTag = getAndIncrementNextReadTag()
    recordPendingRead(readTag, promise, metrics.readCompletionTime.time())
    logger.trace(s"Sending read request: $readTag")
    sendEvent(EventFromValidator().withRead(Read(tag = readTag.toString, keys = keys)))

    promise.future
      .map { keyValuePairs =>
        logger.trace(s"Key-value pairs received for readTag=$readTag correlationId=$correlationId")
        val kvMap = keyValuePairs.map { pair =>
          pair.key -> pair.value
        }.toMap
        keys.map { key =>
          val optValue = kvMap.get(key)
          val result = optValue.flatMap { v =>
            if (v.isEmpty) None else Some(v)
          }
          recordCompleteHashIfTraceLog(key, result)
          result
        }
      }
  }

  override def write(data: Seq[(Key, Value, AccessControlList)]): Future[Unit] = Future {
    scala.concurrent.blocking {
      this.synchronized {
        pendingWrites.appendAll(data)
      }
      ()
    }
  }

  /**
    * Returns all buffered key-value pairs and clears the buffer.
    * Key-value pairs are ordered according to their key.
    */
  def getAndClearWriteSet(): Seq[ProtectedKeyValuePair] = {
    val protectedKeyValuePairs = scala.concurrent.blocking {
      this.synchronized {
        val sortedKeyValuePairs =
          pendingWrites.sortBy(_._1).map {
            case (key, value, acl) =>
              ProtectedKeyValuePair(
                key,
                value,
                accessControlListToThinReplicaIds(acl)
              )
          }
        pendingWrites.clear()
        sortedKeyValuePairs
      }
    }
    logger.trace(
      s"Cleared write-set buffer of ${protectedKeyValuePairs.size} key-value pairs, correlationId=$correlationId")
    logHashes(protectedKeyValuePairs)
    val writeSetSerializedSize = protectedKeyValuePairs.map(_.serializedSize).sum
    metrics.bytesWritten.update(writeSetSerializedSize)
    protectedKeyValuePairs
  }

  private def logHashes(writeSet: Seq[ProtectedKeyValuePair]): Unit =
    if (logger.isTraceEnabled) {
      val keysHash = hexDigestOfBytes(writeSet.map(_.key))
      val valuesHash = hexDigestOfBytes(writeSet.map(_.value))
      val aclsHash = hexDigestOfStrings(writeSet.flatMap(_.trids))
      logger.trace(
        s"Produced write-set, size=${writeSet.size} keysHash=$keysHash valuesHash=$valuesHash aclsHash=$aclsHash correlationId=$correlationId")
    }

  override protected def extractTag(result: EventToValidator.ReadResult): Int = result.tag.toInt

  override protected def extractReadSet(result: EventToValidator.ReadResult): Seq[KeyValuePair] =
    result.keyValuePairs
}

object ConcordLedgerStateOperations {
  private[engine] val VisibleToThisReplicaOnly: Seq[String] = Seq("_")

  private[engine] def accessControlListToThinReplicaIds(
      accessControlList: AccessControlList): Seq[String] =
    accessControlList match {
      case PublicAccess => Seq.empty
      case RestrictedAccess(participants) if participants.isEmpty => VisibleToThisReplicaOnly
      case RestrictedAccess(participants) => participants.map(toReplicaId).toSeq.sorted
    }
}
