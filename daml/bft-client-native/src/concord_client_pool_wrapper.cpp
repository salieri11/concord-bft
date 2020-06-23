#include <unordered_map>
#include <memory>

#include "concord_client_pool_wrapper.h"

using concord::concord_client_pool::ConcordClientPool;
using concord::concord_client_pool::SubmitResult;
using concord::concord_client_pool::PoolStatus;
using bft::client::RequestConfig;
using bft::client::WriteConfig;

static uint16_t current_id = 0;
static std::unordered_map<uint16_t, std::unique_ptr<ConcordClientPool>> client_handles;

uint16_t BFTClient_create(const char *config_file_path) {
    uint16_t new_client_handle = current_id++;
    client_handles.emplace(new_client_handle, std::make_unique<ConcordClientPool>(std::string(config_file_path)));
    return new_client_handle;
}

WriteConfig createWriteConfig(bool pre_execute, unsigned long timeout_millis, const char* correlation_id) {
    RequestConfig requestConfig;

    requestConfig.pre_execute = pre_execute;
    requestConfig.timeout = std::chrono::milliseconds(timeout_millis);
    requestConfig.correlation_id = std::string(correlation_id);

    WriteConfig config;
    config.request = requestConfig;

    return config;
}

BFTClient_SubmitResult_t BFTClient_send_request(
    uint16_t client_handle,
    const char *concord_request_protobuf_bytes,
    size_t concord_request_protobuf_bytes_length,
    bool pre_execute,
    unsigned long timeout_millis,
    const char *correlation_id
) {
    return client_handles[client_handle]->SendRequest(
        createWriteConfig(pre_execute, timeout_millis, correlation_id),
        std::vector<char>(
            concord_request_protobuf_bytes,
            concord_request_protobuf_bytes + concord_request_protobuf_bytes_length
        )
    );
}

BFTClient_PoolStatus_t BFTClient_health_status(uint16_t client_handle) {
    return client_handles[client_handle]->HealthStatus();
}

void BFTClient_destroy(uint16_t client_handle) {
    client_handles.erase(client_handle);
}
