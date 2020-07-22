// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.

package com.digitalasset.daml.on.vmware.execution.engine

import java.util.concurrent.atomic.AtomicInteger

import com.codahale.metrics.Timer.Context
import com.daml.ledger.validator.LedgerStateOperations.{Key, Value}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.concurrent.Promise

private[engine] trait ConcordLedgerStateReadHandler[ReadResultType, ReadSetType] {
  import com.digitalasset.daml.on.vmware.execution.engine.ConcordLedgerStateReadHandler._

  protected final case class PendingRead(promise: Promise[Seq[ReadSetType]], timerContext: Context)

  protected[engine] final val nextReadTag = new AtomicInteger
  protected[engine] final val pendingReads = mutable.Map.empty[Int, PendingRead]
  protected[engine] final val correlationId = new mutable.StringBuilder()
  // The below map is only populated if TRACE log-level is enabled for this class.
  protected final val completedReadHashes: TrieMap[Key, ValueHash] =
    scala.collection.concurrent.TrieMap.empty[Key, ValueHash]

  protected final val logger: Logger = LoggerFactory.getLogger(this.getClass)

  /**
    * Updates the currently set correlation ID used for logging purposes.
    * Not thread-safe.
    * @param newCorrelationId correlation ID to set; None if no correlation ID should be logged
    */
  final def updateCorrelationId(newCorrelationId: Option[String]): Unit = {
    correlationId.clear()
    newCorrelationId.map(correlationId.append)
  }

  final def handleReadResult(result: ReadResultType): Unit = {
    val readTag = extractTag(result)
    logger.trace(s"Handling read result for readTag=$readTag correlationId=$correlationId")
    val PendingRead(promise, timerContext) =
      this
        .synchronized {
          pendingReads.remove(readTag)
        }
        .getOrElse(
          sys.error(s"No read request for id $readTag!")
        )
    timerContext.stop()

    val readSet = extractReadSet(result)
    logger.trace(
      s"Completing promise with ${readSet.size} read set elements, correlationId=$correlationId")
    promise.success(readSet)
  }

  private[engine] def getCompletedReadHashes: scala.collection.Map[Key, ValueHash] =
    completedReadHashes.readOnlySnapshot()

  protected def getAndIncrementNextReadTag(): Int =
    nextReadTag.getAndIncrement()

  protected def recordPendingRead(
      readTag: Int,
      promise: Promise[Seq[ReadSetType]],
      timerContext: Context): Unit =
    this.synchronized {
      pendingReads(readTag) = PendingRead(promise, timerContext)
    }

  protected def recordCompleteHashIfTraceLog(key: Key, result: Option[Value]): Unit =
    if (logger.isTraceEnabled) {
      result.foreach(value => completedReadHashes.put(key, Digests.digestOfByteString(value)))
    }

  protected def extractTag(result: ReadResultType): Int

  protected def extractReadSet(result: ReadResultType): Seq[ReadSetType]
}

object ConcordLedgerStateReadHandler {
  private type ValueHash = Array[Byte]
}
