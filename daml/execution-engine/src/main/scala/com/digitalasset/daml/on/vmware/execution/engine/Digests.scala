package com.digitalasset.daml.on.vmware.execution.engine

import java.security.MessageDigest

import com.google.protobuf.ByteString

object Digests {
  private val Algorithm = "MD5"

  def digestOfByteString(bytes: ByteString): Array[Byte] = {
    val messageDigest = MessageDigest.getInstance(Algorithm)
    messageDigest.update(bytes.asReadOnlyByteBuffer())
    messageDigest.digest()
  }

  def digestOfBytes(listOfBytes: Iterable[ByteString]): Array[Byte] = {
    val messageDigest = MessageDigest.getInstance(Algorithm)
    listOfBytes.foreach(bytes => messageDigest.update(bytes.asReadOnlyByteBuffer()))
    messageDigest.digest()
  }

  def hexDigestOfBytes(listOfBytes: Iterable[ByteString]): String =
    digestOfBytes(listOfBytes)
      .map("%02x" format _)
      .mkString

  def hexDigestOfStrings(listOfStrings: Iterable[String]): String = {
    val messageDigest = MessageDigest.getInstance(Algorithm)
    listOfStrings.foreach(string => messageDigest.update(string.getBytes()))
    messageDigest
      .digest()
      .map("%02x" format _)
      .mkString
  }
}
