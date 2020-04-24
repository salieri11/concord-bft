package com.digitalasset.daml.on.vmware.execution.engine

import com.daml.ledger.validator.batch.BatchLedgerOpsWithAccessControl
import com.daml.ledger.validator.privacy.{
  AccessControlList,
  LedgerStateOperationsWithAccessControlV2,
  PublicAccess,
  RestrictedAccess
}
import com.digitalasset.kvbc.daml_validator.{
  EventFromValidator,
  EventToValidator,
  KeyValuePair,
  ProtectedKeyValuePair
}
import com.digitalasset.kvbc.daml_validator.EventFromValidator.{Read, Write}
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, Promise}

class KVBCLedgerOps(sendEvent: EventFromValidator => Unit)(
    implicit val executionContext: ExecutionContext)
    extends LedgerStateOperationsWithAccessControlV2
    with BatchLedgerOpsWithAccessControl {
  private var nextReadTag = 0
  private val readPromises = mutable.Map.empty[Int, Promise[Seq[KeyValuePair]]]

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
    val promise = Promise[Seq[(KeyValuePair)]]()
    val readTag = this.synchronized {
      val readTag = nextReadTag
      nextReadTag += 1
      readPromises(readTag) = promise
      readTag
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

  override def write(data: Seq[(Key, Value, AccessControlList)]): Future[Unit] =
    Future {
      val protectedKeyValuePairs =
        data.sortBy(_._1).map {
          case (key, value, acl) =>
            ProtectedKeyValuePair(
              key,
              value,
              acl match {
                case PublicAccess => Seq.empty
                case RestrictedAccess(participants) =>
                  if (participants.isEmpty) Seq("_")
                  else participants.map(x => x: String).toSeq.sorted
              }
            )
        }
      sendEvent(EventFromValidator().withWrite(Write(protectedKeyValuePairs)))
    }
}
