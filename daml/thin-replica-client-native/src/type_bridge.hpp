// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates.
// All rights reserved.

#ifndef THIN_REPLICA_CLIENT_NATIVE_WRAPPER_
#define THIN_REPLICA_CLIENT_NATIVE_WRAPPER_

#include <jni.h>

#ifdef __cplusplus
extern "C" {
#endif

jboolean initialize(JNIEnv *env, jobject obj, jstring j_client_id,
                    jshort j_max_faulty, jstring j_private_key,
                    jobjectArray j_servers, jshort j_max_read_data_timeout,
                    jshort j_max_read_hash_timeout,
                    jstring j_jaeger_agent_host_port);

jint currentHealth(JNIEnv *env, jobject obj);

jboolean subscribe(JNIEnv *env, jobject obj, jstring j_prefix);
jboolean subscribeFrom(JNIEnv *env, jobject obj, jstring j_prefix,
                       jlong j_block_id);
jboolean unsubscribe(JNIEnv *env, jobject obj);

jobject pop(JNIEnv *env, jobject obj);
jobject tryPop(JNIEnv *env, jobject obj);

jboolean acknowledgeBlockId(JNIEnv *env, jobject obj, jlong j_block_id);

jobject getTestUpdate(JNIEnv *env, jobject obj);

#ifdef __cplusplus
}
#endif

#endif
