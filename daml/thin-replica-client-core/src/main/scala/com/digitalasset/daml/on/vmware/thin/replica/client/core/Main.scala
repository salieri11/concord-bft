// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
package com.digitalasset.daml.on.vmware.thin.replica.client.core

import com.google.protobuf.ByteString
import com.daml.ledger.participant.state.kvutils._

object Main extends App {

  def isEqual(a: Update, b: Update): Boolean =
    a.toString == b.toString
  def isEqual(a: Option[Update], b: Option[Update]): Boolean =
    (a, b) match {
      case (Some(e1), Some(e2)) => isEqual(e1, e2)
      case _ => false
    }

  def printUpdate(update: Update): Unit = {
    if (update.kvPairs.length > 0) {
      System.out.println(s"Processing ${update.blockId} ${update.kvPairs.length}")
      update.kvPairs.map {
        case Tuple2(entryId, value) =>
          try {
            System.out.println(entryId)
            val logEntry =
              Envelope.open(ByteString.copyFrom(value)) match {
                case Right(Envelope.LogEntryMessage(logEntry)) =>
                  System.out.println("Envelope opened")
                  logEntry
                case _ =>
                  sys.error(s"Envelope did not contain log entry")
              }
          } catch {
            case e: RuntimeException =>
              System.out.println(
                s"Processing block failed with an exception, error='${e.toString}'")
              sys.error(e.toString)
          }
        case _ => System.out.println("Some other stuff")
      }
    }
  }

  @volatile var keepRunning = true

  val mainThread = Thread.currentThread();
  Runtime.getRuntime.addShutdownHook(new Thread(() => {
    System.out.println("Shutting down.")
    keepRunning = false
    mainThread.join()
  }))

  val u = Library.getTestUpdate
  System.out.println(u)
  assert(
    isEqual(
      u,
      Some(Update(17, Array("Alice".getBytes -> "Bob".getBytes), "test", "SpanContext".getBytes))))

  val creatResult = Library.createTRC(
    "example_client_id",
    1,
    "",
    Array[String]("concord1:50051", "concord2:50051", "concord3:50051", "concord4:50051"),
    5,
    5,
    "localhost:6831")
  assert(creatResult == true)
  System.out.println("ThinReplicaClient constructed.")

  val subsResult = Library.subscribe("daml")
  assert(creatResult == true)
  System.out.println("ThinReplicaClient subscribed.")

  var update = Library.tryPop()
  val has_update = update.nonEmpty
  var latest_block_id: Long = 0
  if (!has_update) {
    System.out.println("Subscription call did not yield any updates as initial state.")
  } else {
    System.out.println(
      "The subscribe appears to have returned initial state to the update " +
        "queue; fetching state from the update queue...")
  }

  while (keepRunning && update.nonEmpty) {
    printUpdate(update.get)
    latest_block_id = update.get.blockId
    update = Library.tryPop()
  }

  if (has_update) {
    System.out.println(
      "The (at least initial) contents of the update queue have been " +
        "exhausted; will now wait for and report any additional updates...")
    Library.acknowledgeBlockId(latest_block_id)
    System.out.println("Update(s) acknowledged.")

  } else {
    System.out.println("Will wait for and report any updates...")
  }

  update = Library.pop()
  while (keepRunning && update.nonEmpty) {
    printUpdate(update.get)
    latest_block_id = update.get.blockId
    Library.acknowledgeBlockId(latest_block_id)
    //System.out.println(s"Acknowledged update with with Block ID $latest_block_id.")

    update = Library.pop()
  }

  System.out.println("It appears the update consumer thread was recalled.")

  Library.unsubscribe()
  System.out.println("ThinReplicaClient unsubscribed.")
}
