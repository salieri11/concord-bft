// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates.
// All rights reserved.

#include <stdio.h>
#include "com_digitalasset_daml_on_vmware_thin_replica_client_core_Library__.h"

#include "type_bridge.hpp"

/*
 * Class: com.digitalasset.daml.on.vmware.thin.replica.client.core.Library_00024
 * Method:    createTRC
 * Signature: (Ljava/lang/String;SLjava/lang/String;[Ljava/lang/String;)Z
 */
JNIEXPORT jboolean JNICALL
Java_com_digitalasset_daml_on_vmware_thin_replica_client_core_Library_00024_createTRC(
    JNIEnv *env, jobject obj, jstring client_id, jshort max_faulty,
    jstring private_key, jobjectArray servers) {
  return createTRC(env, obj, client_id, max_faulty, private_key, servers);
}

/*
 * Class: com.digitalasset.daml.on.vmware.thin.replica.client.core.Library_00024
 * Method:    subscribe
 * Signature: (Ljava/lang/String;)Z
 */
JNIEXPORT jboolean JNICALL
Java_com_digitalasset_daml_on_vmware_thin_replica_client_core_Library_00024_subscribe__Ljava_lang_String_2(
    JNIEnv *env, jobject obj, jstring prefix) {
  return subscribe(env, obj, prefix);
}

/*
 * Class: com.digitalasset.daml.on.vmware.thin.replica.client.core.Library_00024
 * Method:    subscribe
 * Signature: (Ljava/lang/String;J)Z
 */
JNIEXPORT jboolean JNICALL
Java_com_digitalasset_daml_on_vmware_thin_replica_client_core_Library_00024_subscribe__Ljava_lang_String_2J(
    JNIEnv *env, jobject obj, jstring prefix, jlong last_known_block_id) {
  return subscribeFrom(env, obj, prefix, last_known_block_id);
}

/*
 * Class: com.digitalasset.daml.on.vmware.thin.replica.client.core.Library_00024
 * Method:    unsubscribe
 * Signature: ()Z
 */
JNIEXPORT jboolean JNICALL
Java_com_digitalasset_daml_on_vmware_thin_replica_client_core_Library_00024_unsubscribe(
    JNIEnv *env, jobject obj) {
  return unsubscribe(env, obj);
}

/*
 * Class: com.digitalasset.daml.on.vmware.thin.replica.client.core.Library_00024
 * Method:    pop
 * Signature: ()Lscala/Option;
 */
JNIEXPORT jobject JNICALL
Java_com_digitalasset_daml_on_vmware_thin_replica_client_core_Library_00024_pop(
    JNIEnv *env, jobject obj) {
  return pop(env, obj);
}

/*
 * Class: com.digitalasset.daml.on.vmware.thin.replica.client.core.Library_00024
 * Method:    tryPop
 * Signature: ()Lscala/Option;
 */
JNIEXPORT jobject JNICALL
Java_com_digitalasset_daml_on_vmware_thin_replica_client_core_Library_00024_tryPop(
    JNIEnv *env, jobject obj) {
  return tryPop(env, obj);
}

/*
 * Class: com.digitalasset.daml.on.vmware.thin.replica.client.core.Library_00024
 * Method:    acknowledgeBlockId
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_com_digitalasset_daml_on_vmware_thin_replica_client_core_Library_00024_acknowledgeBlockId(
    JNIEnv *env, jobject obj, jlong block_id) {
  return acknowledgeBlockId(env, obj, block_id);
}

/*
 * Class: com.digitalasset.daml.on.vmware.thin.replica.client.core.Library_00024
 * Method:    getTestUpdate
 * Signature: ()Lscala/Option;
 */
JNIEXPORT jobject JNICALL
Java_com_digitalasset_daml_on_vmware_thin_replica_client_core_Library_00024_getTestUpdate(
    JNIEnv *env, jobject obj) {
  return getTestUpdate(env, obj);
}