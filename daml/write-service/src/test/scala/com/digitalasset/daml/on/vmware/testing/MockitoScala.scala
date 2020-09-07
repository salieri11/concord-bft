package com.digitalasset.daml.on.vmware.testing

import org.mockito.ArgumentCaptor
import org.scalatestplus.mockito.MockitoSugar

import scala.reflect.ClassTag

trait MockitoScala extends MockitoSugar {
  def captor[T: ClassTag]: ArgumentCaptor[T] =
    ArgumentCaptor.forClass[T, T](implicitly[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]])
}

object MockitoScala extends MockitoScala
