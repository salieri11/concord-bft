package com.digitalasset.daml.on.vmware.execution.engine

import java.util.concurrent.atomic.AtomicInteger

import com.codahale.metrics.Timer.Context
import com.codahale.metrics.{Histogram, MetricRegistry, Timer}
import com.daml.ledger.validator.privacy.{
  AccessControlList,
  LedgerStateOperationsWithAccessControl,
  PublicAccess,
  RestrictedAccess
}
import com.daml.metrics.MetricName
import com.digitalasset.kvbc.daml_validator.EventFromValidator.{Read, Write}
import com.digitalasset.kvbc.daml_validator.{
  EventFromValidator,
  EventToValidator,
  KeyValuePair,
  ProtectedKeyValuePair
}
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, Promise}

class ConcordLedgerStateOperations(
    sendEvent: EventFromValidator => Unit,
    metrics: ConcordLedgerStateOperations.Metrics)(implicit val executionContext: ExecutionContext)
    extends LedgerStateOperationsWithAccessControl {
  import ConcordLedgerStateOperations.accessControlListToThinReplicaIds

  private[engine] case class PendingRead(promise: Promise[Seq[KeyValuePair]], timerContext: Context)

  private[engine] val nextReadTag = new AtomicInteger
  private[engine] val pendingReads = mutable.Map.empty[Int, PendingRead]
  private[engine] val pendingWrites = mutable.Buffer[(Key, Value, AccessControlList)]()
  private[engine] val correlationId = new mutable.StringBuilder()

  private val logger = LoggerFactory.getLogger(this.getClass)

  /**
    * Updates the currently set correlation ID used for logging purposes.
    * Not thread-safe.
    * @param newCorrelationId correlation ID to set; None if no correlation ID should be logged
    */
  def updateCorrelationId(newCorrelationId: Option[String]): Unit = {
    correlationId.clear()
    newCorrelationId.map(correlationId.append)
  }

  def handleReadResult(result: EventToValidator.ReadResult): Unit = {
    val readTag = result.tag.toInt
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

    logger.trace(
      s"Completing promise with ${result.keyValuePairs.size} key-value pairs, correlationId=$correlationId")
    promise.success(result.keyValuePairs)
  }

  override def read(keys: Seq[Key]): Future[Seq[Option[Value]]] = {
    // Create a promise that we'll fulfill from [[handleReadResult]] when the
    // result arrives.
    val promise = Promise[Seq[KeyValuePair]]()
    val readTag = nextReadTag.getAndIncrement()
    this.synchronized {
      pendingReads(readTag) = PendingRead(promise, metrics.readCompletionTime.time())
    }
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
          optValue.flatMap { v =>
            if (v.isEmpty) None else Some(v)
          }
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
    * Sends all buffered key-value pairs to the Command Handler and clears the buffer.
    * Key-value pairs are sent ordered according to their key.
    */
  def flushWrites(): Future[Unit] = Future {
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
    logger.trace(s"Sending write-set of ${protectedKeyValuePairs.size} key-value pairs, correlationId=$correlationId")
    val writeSet = Write(protectedKeyValuePairs)
    sendEvent(EventFromValidator().withWrite(writeSet))
    metrics.bytesWritten.update(writeSet.serializedSize)
  }
}

object ConcordLedgerStateOperations {
  private[engine] val VisibleToThisReplicaOnly: Seq[String] = Seq("_")

  private[engine] def accessControlListToThinReplicaIds(
      accessControlList: AccessControlList): Seq[String] =
    accessControlList match {
      case PublicAccess => Seq.empty
      case RestrictedAccess(participants) if participants.isEmpty => VisibleToThisReplicaOnly
      case RestrictedAccess(participants) => participants.map(x => x: String).toSeq.sorted
    }

  private[engine] class Metrics(metricRegistry: MetricRegistry) {
    val Prefix: MetricName = MetricName.DAML :+ "validator"

    val readCompletionTime: Timer = metricRegistry.timer(Prefix :+ "key_read")
    val bytesWritten: Histogram = metricRegistry.histogram(Prefix :+ "bytes_written")
  }
}
