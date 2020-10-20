// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates.
// All rights reserved.

#include <stdio.h>

#include "com_digitalasset_daml_on_vmware_thin_replica_client_core_ThinReplicaClientJni.h"
#include "type_bridge.hpp"

/*
 * Class:
 * com.digitalasset.daml.on.vmware.thin.replica.client.core.ThinReplicaClientJni
 * Method:    initialize
 * Signature: (Ljava/lang/String;SLjava/lang/String;[Ljava/lang/String;)Z
 */
JNIEXPORT jboolean JNICALL
Java_com_digitalasset_daml_on_vmware_thin_replica_client_core_ThinReplicaClientJni_initialize(
    JNIEnv *env, jobject obj, jstring client_id, jshort max_faulty,
    jstring private_key, jobjectArray servers, jshort max_read_data_timeout,
    jshort max_read_hash_timeout, jstring jaeger_agent_host_port) {
  return initialize(env, obj, client_id, max_faulty, private_key, servers,
                    max_read_data_timeout, max_read_hash_timeout,
                    jaeger_agent_host_port);
}

/*
 * Class:
 * com.digitalasset.daml.on.vmware.thin.replica.client.core.ThinReplicaClientJni
 * Method:    currentHealth
 * Signature: ()I
 */
JNIEXPORT jint JNICALL
Java_com_digitalasset_daml_on_vmware_thin_replica_client_core_ThinReplicaClientJni_currentHealth(
    JNIEnv *env, jobject obj) {
  return currentHealth(env, obj);
}

/*
 * Class:
 * com.digitalasset.daml.on.vmware.thin.replica.client.core.ThinReplicaClientJni
 * Method:    subscribe
 * Signature: (Ljava/lang/String;)Z
 */
JNIEXPORT jboolean JNICALL
Java_com_digitalasset_daml_on_vmware_thin_replica_client_core_ThinReplicaClientJni_subscribe__Ljava_lang_String_2(
    JNIEnv *env, jobject obj, jstring prefix) {
  return subscribe(env, obj, prefix);
}

/*
 * Class:
 * com.digitalasset.daml.on.vmware.thin.replica.client.core.ThinReplicaClientJni
 * Method:    subscribe
 * Signature: (Ljava/lang/String;J)Z
 */
JNIEXPORT jboolean JNICALL
Java_com_digitalasset_daml_on_vmware_thin_replica_client_core_ThinReplicaClientJni_subscribe__Ljava_lang_String_2J(
    JNIEnv *env, jobject obj, jstring prefix, jlong block_id) {
  return subscribeFrom(env, obj, prefix, block_id);
}

/*
 * Class:
 * com.digitalasset.daml.on.vmware.thin.replica.client.core.ThinReplicaClientJni
 * Method:    unsubscribe
 * Signature: ()Z
 */
JNIEXPORT jboolean JNICALL
Java_com_digitalasset_daml_on_vmware_thin_replica_client_core_ThinReplicaClientJni_unsubscribe(
    JNIEnv *env, jobject obj) {
  return unsubscribe(env, obj);
}

/*
 * Class:
 * com.digitalasset.daml.on.vmware.thin.replica.client.core.ThinReplicaClientJni
 * Method:    pop
 * Signature: ()Lscala/Option;
 */
JNIEXPORT jobject JNICALL
Java_com_digitalasset_daml_on_vmware_thin_replica_client_core_ThinReplicaClientJni_pop(
    JNIEnv *env, jobject obj) {
  return pop(env, obj);
}

/*
 * Class:
 * com.digitalasset.daml.on.vmware.thin.replica.client.core.ThinReplicaClientJni
 * Method:    tryPop
 * Signature: ()Lscala/Option;
 */
JNIEXPORT jobject JNICALL
Java_com_digitalasset_daml_on_vmware_thin_replica_client_core_ThinReplicaClientJni_tryPop(
    JNIEnv *env, jobject obj) {
  return tryPop(env, obj);
}

/*
 * Class:
 * com.digitalasset.daml.on.vmware.thin.replica.client.core.ThinReplicaClientJni
 * Method:    acknowledgeBlockId
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_com_digitalasset_daml_on_vmware_thin_replica_client_core_ThinReplicaClientJni_acknowledgeBlockId(
    JNIEnv *env, jobject obj, jlong block_id) {
  return acknowledgeBlockId(env, obj, block_id);
}

/*
 * Class:
 * com.digitalasset.daml.on.vmware.thin.replica.client.core.ThinReplicaClientJni
 * Method:    getTestUpdate
 * Signature: ()Lscala/Option;
 */
JNIEXPORT jobject JNICALL
Java_com_digitalasset_daml_on_vmware_thin_replica_client_core_ThinReplicaClientJni_getTestUpdate(
    JNIEnv *env, jobject obj) {
  return getTestUpdate(env, obj);
}
