package com.digitalasset.daml.on.vmware.execution.engine

import java.util.concurrent.atomic.AtomicInteger

import com.daml.ledger.validator.privacy.{
  AccessControlList,
  LedgerStateOperationsWithAccessControl,
  PublicAccess,
  RestrictedAccess
}
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

class ConcordLedgerStateOperations(sendEvent: EventFromValidator => Unit)(
    implicit val executionContext: ExecutionContext)
    extends LedgerStateOperationsWithAccessControl {
  import ConcordLedgerStateOperations.accessControlListToThinReplicaIds

  private[engine] val nextReadTag = new AtomicInteger
  private[engine] val readPromises = mutable.Map.empty[Int, Promise[Seq[KeyValuePair]]]

  private val logger = LoggerFactory.getLogger(this.getClass)

  def handleReadResult(result: EventToValidator.ReadResult): Unit = {
    val readTag = result.tag.toInt
    logger.trace(s"Handling read result for $readTag")

    val promise =
      this
        .synchronized {
          readPromises.remove(readTag)
        }
        .getOrElse(
          sys.error(s"No read request for id $readTag!")
        )

    logger.trace(s"Completing promise with ${result.keyValuePairs.size} key-value pairs")
    promise.success(result.keyValuePairs)
  }

  override def read(keys: Seq[Key]): Future[Seq[Option[Value]]] = {
    // Create a promise that we'll fulfill from [[handleReadResult]] when the
    // result arrives.
    val promise = Promise[Seq[KeyValuePair]]()
    val readTag = nextReadTag.getAndIncrement()
    this.synchronized {
      readPromises(readTag) = promise
    }

    logger.trace(s"Sending read request: $readTag")

    sendEvent(EventFromValidator().withRead(Read(tag = readTag.toString, keys = keys)))

    promise.future
      .map { keyValuePairs =>
        logger.trace(s"Key-value pairs received for $readTag")
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
    val protectedKeyValuePairs =
      data.sortBy(_._1).map {
        case (key, value, acl) =>
          ProtectedKeyValuePair(
            key,
            value,
            accessControlListToThinReplicaIds(acl)
          )
      }
    sendEvent(EventFromValidator().withWrite(Write(protectedKeyValuePairs)))
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
}
