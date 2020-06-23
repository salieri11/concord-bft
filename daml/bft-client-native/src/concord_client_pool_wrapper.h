#ifndef __CONCORD_CLIENT_POOL_WRAPPER_H__
#define __CONCORD_CLIENT_POOL_WRAPPER_H__

#include "external/concord_client_pool.hpp"

using concord::concord_client_pool::PoolStatus;
using concord::concord_client_pool::SubmitResult;

#ifdef __cplusplus
extern "C" {
#endif

typedef enum SubmitResult BFTClient_SubmitResult_t;
typedef enum PoolStatus BFTClient_PoolStatus_t;

uint16_t BFTClient_create(const char *config_file_path);

BFTClient_SubmitResult_t BFTClient_send_request(
    uint16_t client_handle,
    const char *concord_request_protobuf_bytes,
    size_t concord_request_protobuf_bytes_length,
    bool pre_execute,
    unsigned long timeout_millis,
    const char *correlation_id
);

BFTClient_PoolStatus_t BFTClient_health_status(uint16_t client_handle);

void BFTClient_destroy(uint16_t client_handle);

#ifdef __cplusplus
}
#endif

#endif /* __CONCORD_CLIENT_POOL_WRAPPER_H__ */
