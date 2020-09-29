// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates.
// All rights reserved.

#include "type_bridge.hpp"
#include <algorithm>
#include <exception>
#include <functional>
#include <memory>
#include <string>
#include <thin_replica_client/thin_replica_client_facade.hpp>
#include <vector>

using std::exception;
using std::make_pair;
using std::pair;
using std::string;
using std::transform;
using std::unique_ptr;
using std::vector;
using thin_replica_client::ThinReplicaClientFacade;
using thin_replica_client::Update;

class JNIClass {
 private:
  typedef jobject (JNIEnv::*CtorPtr)(jclass, jmethodID, ...);

  JNIEnv* env;
  jclass javaClass;
  jmethodID javaCtor;
  CtorPtr ctorPtr;

 public:
  // Java classes are instantiated using <init> method
  JNIClass(JNIEnv* env, const string& name, const string& ctorSignature)
      : env(env) {
    // jclass is a local reference. A local reference survives, at most,
    // the duration of a single call to a JNI function, you have to ask the
    // JVM to make a global reference for you.
    jclass temp = env->FindClass(name.c_str());
    javaClass = (jclass)env->NewGlobalRef(temp);
    javaCtor = env->GetMethodID(javaClass, "<init>", ctorSignature.c_str());
    ctorPtr = &JNIEnv::NewObject;
  }

  // Scala classes are instantiated using a dedicated static method, ususally
  // is an 'apply'.
  JNIClass(JNIEnv* env, const string& name, const string& ctor,
           const string& ctorSignature)
      : env(env) {
    jclass temp = env->FindClass(name.c_str());
    javaClass = (jclass)env->NewGlobalRef(temp);
    javaCtor =
        env->GetStaticMethodID(javaClass, ctor.c_str(), ctorSignature.c_str());
    ctorPtr = &JNIEnv::CallStaticObjectMethod;
  }

  // Call a constructor with no arguments
  jobject CreateInstance() const {
    return (env->*ctorPtr)(javaClass, javaCtor);
  }

  // Call a constructor with a single argument
  template <typename T1>
  jobject CreateInstance(const T1& a1) const {
    return (env->*ctorPtr)(javaClass, javaCtor, a1);
  }

  // Call a constructor with two arguments
  template <typename T1, typename T2>
  jobject CreateInstance(const T1& a1, const T2& a2) const {
    return (env->*ctorPtr)(javaClass, javaCtor, a1, a2);
  }

  // Call a constructor with three arguments
  template <typename T1, typename T2, typename T3>
  jobject CreateInstance(const T1& a1, const T2& a2, const T3& a3) const {
    return (env->*ctorPtr)(javaClass, javaCtor, a1, a2, a3);
  }

  // Call a constructor with four arguments
  template <typename T1, typename T2, typename T3, typename T4>
  jobject CreateInstance(const T1& a1, const T2& a2, const T3& a3,
                         const T4& a4) const {
    return (env->*ctorPtr)(javaClass, javaCtor, a1, a2, a3, a4);
  }

  jobjectArray CreateArray(size_t size) const {
    return env->NewObjectArray(size, javaClass, NULL);
  }
};

class JNIConverter {
 private:
  JNIEnv* env;
  unique_ptr<JNIClass> option;
  unique_ptr<JNIClass> none;
  unique_ptr<JNIClass> long_;
  unique_ptr<JNIClass> tuple;
  unique_ptr<JNIClass> update;
  jclass stringClass;

 public:
  JNIConverter(JNIEnv* env)
      : env(env),
        option(CreateClass("scala/Option", "apply",
                           "(Ljava/lang/Object;)Lscala/Option;")),
        none(CreateClass("scala/Option", "empty", "()Lscala/Option;")),
        long_(CreateClass("java/lang/Long", "(J)V")),
        tuple(CreateClass(
            "scala/Tuple2", "apply",
            "(Ljava/lang/Object;Ljava/lang/Object;)Lscala/Tuple2;")),
        update(CreateClass(
            "com/digitalasset/daml/on/vmware/thin/replica/client/core/Update",
            "apply",
            "(J[Lscala/Tuple2;Ljava/lang/String;[B)Lcom/digitalasset/daml/on/"
            "vmware/thin/replica/"
            "client/core/Update;")),
        stringClass(env->FindClass("java/lang/String")) {}

  JNIEnv* GetEnv() const { return env; }

  JNIClass* CreateClass(const string& name, const string& ctorSignature) const {
    return new JNIClass(env, name, ctorSignature);
  }

  JNIClass* CreateClass(const string& name, const string& ctor,
                        const string& ctorSignature) const {
    return new JNIClass(env, name, ctor, ctorSignature);
  }

  string ToString(jstring jStr) const {
    const char* cStr = env->GetStringUTFChars(jStr, NULL);
    string str = std::string(cStr);
    env->ReleaseStringUTFChars(jStr, cStr);
    return str;
  }

  vector<string> ToStringVect(jobjectArray jstringArray) const {
    vector<string> strs;
    int stringCount = env->GetArrayLength(jstringArray);
    strs.reserve(stringCount);

    for (int i = 0; i < stringCount; i++) {
      jstring js = (jstring)(env->GetObjectArrayElement(jstringArray, i));
      strs.push_back(ToString(js));
    }
    return strs;
  }

  jobject CreateOption(jobject payload) const {
    if (payload)
      return option->CreateInstance(payload);
    else
      return none->CreateInstance();
  }

  jobject CreateLong(long val) const { return long_->CreateInstance(val); }

  jstring CreateString(const char* str) const {
    return (env->NewStringUTF(str));
  }

  jstring CreateString(const std::string& str) const {
    return (env->NewStringUTF(str.c_str()));
  }

  jbyteArray CreateByteArray(const std::string& str) const {
    jbyteArray jBuff = env->NewByteArray(str.length());
    env->SetByteArrayRegion(jBuff, 0, str.length(), (jbyte*)str.c_str());
    return jBuff;
  }

  jobjectArray CreateStringArray(size_t size) const {
    return env->NewObjectArray(size, stringClass, NULL);
  }

  jobjectArray CreateStringArray(const vector<string>& strs) const {
    jobjectArray array = env->NewObjectArray(strs.size(), stringClass, NULL);
    for (size_t i = 0; i < strs.size(); i++)
      env->SetObjectArrayElement(array, i, CreateString(strs[i]));
    return array;
  }

  jobject CreateTuple(jobject o1, jobject o2) const {
    return tuple->CreateInstance(o1, o2);
  }

  jobjectArray CreateTupleArray(size_t size) const {
    return tuple->CreateArray(size);
  }

  jobject CreateUpdate(long blockId, jobject kvPairs, jstring correlationId,
                       jbyteArray spanContext) const {
    return update->CreateInstance(blockId, kvPairs, correlationId, spanContext);
  }
};

// Querying JNI type information is expensive. It should not be repeated every
// time a JNI call is made. We keep this information cached inside the
// JNIConverter class. However, JNI objects cannot be shared between threads,
// so a copy of JNIConverter has to be kept per thread local storage.
// JNIConverterFactory hides this detail in its implementation.
class JNIConverterFactory {
 public:
  static JNIConverter* CreateInstance(JNIEnv* env) {
    thread_local unique_ptr<JNIConverter> converter;
    if (!converter) converter.reset(new JNIConverter(env));
    return converter.get();
  }
};

// There can only be one ThinReplicaClient in any given process
// This class hides the complexity of dealing with the singleton
// aspect of the TRC from the calling code;
class TRCFFactory {
 private:
  typedef unique_ptr<ThinReplicaClientFacade> Instance;
  typedef std::function<void(Instance&)> InstanceModifier;

  static ThinReplicaClientFacade* ModifyAndGet(InstanceModifier modifier) {
    static Instance trcf;
    modifier(trcf);
    return trcf.get();
  }

 public:
  static ThinReplicaClientFacade* GetInstance() {
    return ModifyAndGet([](Instance&) {});
  }

  static ThinReplicaClientFacade* CreateInstance(
      JNIConverter* converter, jstring j_client_id, jshort j_max_faulty,
      jstring j_private_key, jobjectArray j_servers,
      jshort j_max_read_data_timeout, jshort j_max_read_hash_timeout,
      jstring j_jaeger_agent_host_port) {
    return ModifyAndGet([converter, j_client_id, j_max_faulty, j_private_key,
                         j_servers, j_max_read_data_timeout,
                         j_max_read_hash_timeout,
                         j_jaeger_agent_host_port](Instance& instance) {
      if (!instance && converter) {
        string client_id = converter->ToString(j_client_id);
        string private_key = converter->ToString(j_private_key);
        vector<string> servers = converter->ToStringVect(j_servers);
        string jaeger_agent_host_port =
            converter->ToString(j_jaeger_agent_host_port);
        instance.reset(new ThinReplicaClientFacade(
            client_id, j_max_faulty, private_key, servers,
            j_max_read_data_timeout, j_max_read_hash_timeout,
            jaeger_agent_host_port));
      }
    });
  }
};

extern "C" jboolean initialize(JNIEnv* env, jobject obj, jstring j_client_id,
                               jshort j_max_faulty, jstring j_private_key,
                               jobjectArray j_servers,
                               jshort j_max_read_data_timeout,
                               jshort j_max_read_hash_timeout,
                               jstring j_jaeger_agent) {
  ThinReplicaClientFacade* trcf = NULL;
  try {
    JNIConverter* converter = JNIConverterFactory::CreateInstance(env);
    trcf = TRCFFactory::CreateInstance(
        converter, j_client_id, j_max_faulty, j_private_key, j_servers,
        j_max_read_data_timeout, j_max_read_hash_timeout, j_jaeger_agent);
  } catch (const exception& e) {
    return JNI_FALSE;
  }

  return (trcf != NULL) ? JNI_TRUE : JNI_FALSE;
}

extern "C" jboolean subscribe(JNIEnv* env, jobject obj, jstring j_prefix) {
  ThinReplicaClientFacade* trcf = TRCFFactory::GetInstance();
  if (!trcf) return JNI_FALSE;
  JNIConverter* converter = JNIConverterFactory::CreateInstance(env);
  string prefix = converter->ToString(j_prefix);
  try {
    trcf->Subscribe(prefix);
  } catch (const exception&) {
    return JNI_FALSE;
  }
  return JNI_TRUE;
}

extern "C" jboolean subscribeFrom(JNIEnv* env, jobject obj, jstring j_prefix,
                                  jlong j_block_id) {
  ThinReplicaClientFacade* trcf = TRCFFactory::GetInstance();
  if (!trcf) return JNI_FALSE;
  JNIConverter* converter = JNIConverterFactory::CreateInstance(env);
  string prefix = converter->ToString(j_prefix);
  try {
    trcf->Subscribe(prefix, j_block_id);
  } catch (const exception&) {
    return JNI_FALSE;
  }
  return JNI_TRUE;
}

extern "C" jboolean unsubscribe(JNIEnv* env, jobject obj) {
  ThinReplicaClientFacade* trcf = TRCFFactory::GetInstance();
  if (!trcf) return JNI_FALSE;
  try {
    trcf->Unsubscribe();
  } catch (const exception&) {
    return JNI_FALSE;
  }
  return JNI_TRUE;
}

jobject processUpdate(JNIEnv* env, JNIConverter* converter,
                      const unique_ptr<Update>& update) {
  jobjectArray kvPairs = converter->CreateTupleArray(update->kv_pairs.size());
  typedef vector<pair<string, string>> Pairs;
  size_t i = 0;
  for (Pairs::const_iterator it = update->kv_pairs.begin();
       it != update->kv_pairs.end(); ++it) {
    jobject tup =
        converter->CreateTuple(converter->CreateByteArray(it->first),
                               converter->CreateByteArray(it->second));
    env->SetObjectArrayElement(kvPairs, i++, tup);
  }
  jstring correlationId = converter->CreateString(update->correlation_id_);
  jbyteArray spanContext = converter->CreateByteArray(update->span_context);
  jobject u = converter->CreateUpdate(update->block_id, kvPairs, correlationId,
                                      spanContext);
  return converter->CreateOption(u);
}

extern "C" jobject pop(JNIEnv* env, jobject obj) {
  ThinReplicaClientFacade* trcf = TRCFFactory::GetInstance();
  JNIConverter* converter = JNIConverterFactory::CreateInstance(env);
  if (!trcf) return converter->CreateOption(NULL);
  unique_ptr<Update> update;
  try {
    update = trcf->Pop();
  } catch (const exception&) {
  }
  if (!update) return converter->CreateOption(NULL);
  return processUpdate(env, converter, update);
}

extern "C" jobject tryPop(JNIEnv* env, jobject obj) {
  ThinReplicaClientFacade* trcf = TRCFFactory::GetInstance();
  JNIConverter* converter = JNIConverterFactory::CreateInstance(env);
  if (!trcf) return converter->CreateOption(NULL);
  unique_ptr<Update> update;
  try {
    update = trcf->TryPop();
  } catch (const exception&) {
  }
  if (!update) return converter->CreateOption(NULL);
  return processUpdate(env, converter, update);
}

extern "C" jboolean acknowledgeBlockId(JNIEnv* env, jobject obj,
                                       jlong j_block_id) {
  ThinReplicaClientFacade* trcf = TRCFFactory::GetInstance();
  if (!trcf) return JNI_FALSE;
  try {
    trcf->AcknowledgeBlockID(j_block_id);
  } catch (const exception&) {
    return JNI_FALSE;
  }
  return JNI_TRUE;
}

extern "C" jobject getTestUpdate(JNIEnv* env, jobject obj) {
  JNIConverter* converter = JNIConverterFactory::CreateInstance(env);
  jobjectArray kvPairs = converter->CreateTupleArray(1);
  if (!kvPairs) return converter->CreateOption(NULL);
  jbyteArray alice = converter->CreateByteArray("Alice");
  jbyteArray bob = converter->CreateByteArray("Bob");
  jobject tup = converter->CreateTuple(alice, bob);
  jstring correlationId = converter->CreateString("test");
  jbyteArray spanContext = converter->CreateByteArray("SpanContext");
  env->SetObjectArrayElement(kvPairs, 0, tup);
  jobject u = converter->CreateUpdate(17, kvPairs, correlationId, spanContext);
  return converter->CreateOption(u);
}
