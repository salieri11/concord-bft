// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates.
// All rights reserved.

#include <cstring>

#include <jni.h>

#include "com_digitalasset_daml_on_vmware_write_service_bft_BftConcordClientPoolJni.h"

#include "concord_client_pool_wrapper.h"

#ifdef __cplusplus
extern "C" {
#endif

uint16_t get_native_handle(JNIEnv *env, jobject obj);
char *copy_jstring(JNIEnv *env, jstring jstr);

/*
 * Class:     com.digitalasset.daml.on.vmware.write.service.bft.BftConcordClientPoolJni
 * Method:    initialize
 * Signature: (Ljava/lang/String;)S
 */
JNIEXPORT jshort JNICALL Java_com_digitalasset_daml_on_vmware_write_service_bft_BftConcordClientPoolJni_initialize
  (JNIEnv *env, jobject obj, jstring configPath) {
    const char *configPathString = env->GetStringUTFChars(configPath, NULL);
    uint16_t client_handle = BFTClient_create(configPathString);
    env->ReleaseStringUTFChars(configPath, configPathString);

    jshort nativeHandle;
    std::memcpy(&nativeHandle, &client_handle, sizeof(client_handle));
    return nativeHandle;
}

/*
 * Class:     com.digitalasset.daml.on.vmware.write.service.bft.BftConcordClientPoolJni
 * Method:    sendRequest
 * Signature: ([BJZLjava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_com_digitalasset_daml_on_vmware_write_service_bft_BftConcordClientPoolJni_sendRequest
  (JNIEnv *env, jobject obj, jbyteArray req, jlong timeout_millis, jboolean pre_exec, jstring correlationId) {
    uint16_t bftClient = get_native_handle(env, obj);

    const char *correlationIdString = env->GetStringUTFChars(correlationId, NULL);

    jsize reqLen = env->GetArrayLength(req);
    signed char *reqBytes = env->GetByteArrayElements(req, NULL);

    int result = BFTClient_send_request(bftClient, reinterpret_cast<const char *>(reqBytes), reqLen, pre_exec, timeout_millis, correlationIdString);

    env->ReleaseStringUTFChars(correlationId, correlationIdString);
    env->ReleaseByteArrayElements(req, reqBytes, JNI_ABORT);

    return result;
}

/*
 * Class:     com.digitalasset.daml.on.vmware.write.service.bft.BftConcordClientPoolJni
 * Method:    currentHealth
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_com_digitalasset_daml_on_vmware_write_service_bft_BftConcordClientPoolJni_currentHealth
  (JNIEnv *env, jobject obj) {
    uint16_t bftClient = get_native_handle(env, obj);
    return BFTClient_health_status(bftClient);
}

/*
 * Class:     com.digitalasset.daml.on.vmware.write.service.bft.BftConcordClientPoolJni
 * Method:    close
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_com_digitalasset_daml_on_vmware_write_service_bft_BftConcordClientPoolJni_close
  (JNIEnv *env, jobject obj) {
    uint16_t bftClient = get_native_handle(env, obj);
    BFTClient_destroy(bftClient);
}

uint16_t get_native_handle(JNIEnv *env, jobject obj) {
    jclass clazz = env->GetObjectClass(obj);
    jfieldID fieldId = env->GetFieldID(clazz, "nativeHandle", "S");
    jshort nativeHandle = env->GetShortField(obj, fieldId);
    uint16_t bftClient;
    std::memcpy(&bftClient, &nativeHandle, sizeof(bftClient));
    return bftClient;
}

#ifdef __cplusplus
}
#endif
