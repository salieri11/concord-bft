

file (GLOB util_headers "../../submodules/concord-bft/util/include/*.h"
        "../../submodules/concord-bft/util/include/*.hpp")
file (GLOB logging_headers "../../submodules/concord-bft/logging/include/*.hpp")
file (GLOB external_headers "*.hpp"
        "../../submodules/concord-bft/kvbc/include/db_interfaces.h"
        "../../submodules/concord-bft/kvbc/include/kv_types.hpp"
        "../../submodules/concord-bft/kvbc/include/KVBCInterfaces.h"
        "../../submodules/concord-bft/threshsign/src/*.h")
file (GLOB bft_engine_headers "../../submodules/concord-bft/bftengine/include/bftengine/*.hpp")
file (GLOB comm_headers "../../submodules/concord-bft/communication/include/communication/*.h"
        "../../submodules/concord-bft/communication/include/communication/*.hpp")
file (GLOB bft_headers "../../submodules/concord-bft/bftclient/include/bftclient/*.h")

install (FILES ${external_headers} ${util_headers} DESTINATION include/external)
install (FILES ${logging_headers} ../config/configuration_manager.hpp ../config/communication.hpp ../../submodules/concord-bft/threshsign/include/threshsign/ThresholdSignaturesTypes.h DESTINATION include/config)
install (FILES ${bft_engine_headers} ${util_headers} ../../submodules/concord-bft/util/include/Metrics.hpp DESTINATION include/bftengine)
install (FILES ${comm_headers} ${logging_headers} DESTINATION include/communication)
install (FILES ../../submodules/concord-bft/storage/include/storage/db_interface.h ${util_headers} DESTINATION include/storage)
install (FILES ${bft_headers} DESTINATION include/bftclient)
install (FILES ${util_headers} ${logging_headers} DESTINATION include/)


file(GLOB concord_external_client_sources "*.cpp" "*.hpp"
        "../../submodules/concord-bft/threshsign/src/*.cpp")
set(bls_relic_source_files1
        ../../submodules/concord-bft/threshsign/src/bls/relic/BlsAccumulatorBase.cpp
        ../../submodules/concord-bft/threshsign/src/bls/relic/BlsAlmostMultisigAccumulator.cpp
        ../../submodules/concord-bft/threshsign/src/bls/relic/BlsAlmostMultisigCoefficients.cpp
        ../../submodules/concord-bft/threshsign/src/bls/relic/BlsBatchVerifier.cpp
        ../../submodules/concord-bft/threshsign/src/bls/relic/BlsMultisigAccumulator.cpp
        ../../submodules/concord-bft/threshsign/src/bls/relic/BlsMultisigKeygen.cpp
        ../../submodules/concord-bft/threshsign/src/bls/relic/BlsMultisigVerifier.cpp
        ../../submodules/concord-bft/threshsign/src/bls/relic/BlsNumTypes.cpp
        ../../submodules/concord-bft/threshsign/src/bls/relic/BlsPolynomial.cpp
        ../../submodules/concord-bft/threshsign/src/bls/relic/BlsPublicParameters.cpp
        ../../submodules/concord-bft/threshsign/src/bls/relic/BlsThresholdAccumulator.cpp
        ../../submodules/concord-bft/threshsign/src/bls/relic/BlsThresholdAccumulatorBaseExports.cpp
        ../../submodules/concord-bft/threshsign/src/bls/relic/BlsThresholdFactory.cpp
        ../../submodules/concord-bft/threshsign/src/bls/relic/BlsThresholdKeygen.cpp
        ../../submodules/concord-bft/threshsign/src/bls/relic/BlsThresholdSigner.cpp
        ../../submodules/concord-bft/threshsign/src/bls/relic/BlsThresholdVerifier.cpp
        ../../submodules/concord-bft/threshsign/src/bls/relic/FastMultExp.cpp
        ../../submodules/concord-bft/threshsign/src/bls/relic/LagrangeInterpolation.cpp
        ../../submodules/concord-bft/threshsign/src/bls/relic/Library.cpp
        )

set(corebft_source_files1
        ../../submodules/concord-bft/bftengine/src/bcstatetransfer/InMemoryDataStore.cpp
        ../../submodules/concord-bft/bftengine/src/bftengine/messages/MessageBase.cpp
        ../../submodules/concord-bft/bftengine/src/bftengine/messages/ClientReplyMsg.cpp
        ../../submodules/concord-bft/bftengine/src/bftengine/messages/ClientRequestMsg.cpp
        ../../submodules/concord-bft/bftengine/src/preprocessor/messages/ClientPreProcessRequestMsg.cpp
        ../../submodules/concord-bft/bftengine/src/bcstatetransfer/STDigest.cpp
        )
#set(CMAKE_POSITION_INDEPENDENT_CODE ON)
include_directories(${CMAKE_CURRENT_BINARY_DIR}/submodules)
include_directories(${CMAKE_CURRENT_BINARY_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(../../submodules/concord-bft/threshsign/include/)
include_directories(../../submodules/concord-bft/bftengine/include/bcstatetransfer/)
add_library(concord_external_client STATIC
        ${concord_external_client_sources}
        ../config/configuration_manager.cpp
        )
get_property(thresh_include GLOBAL PROPERTY thresh_include_folder)
target_include_directories(concord_external_client PRIVATE ${thresh_include})


target_link_libraries(concord_external_client PUBLIC
        ${Boost_LIBRARIES}
        ${yaml-cpp_LIBRARIES}
        log4cplus
        concord_utils
        bftclient
        bftclient_new
        kvbc
        util
        threshsign)

add_library(concord_external_client_shared SHARED
        ${concord_external_client_sources}
        ${bls_relic_source_files1}
        ${corebft_source_files1}
        ../config/configuration_manager.cpp
        ../../submodules/concord-bft/threshsign/src/ThresholdSignaturesTypes.cpp
        ../../submodules/concord-bft/threshsign/src/bls/relic/BlsNumTypes.cpp
        ../../submodules/concord-bft/logging/src/Logger.cpp
        ../../submodules/concord-bft/threshsign/src/bls/relic/BlsThresholdFactory.cpp
        )
target_include_directories(concord_external_client_shared PRIVATE ${thresh_include})
target_link_libraries(concord_external_client_shared PUBLIC
        ${Boost_LIBRARIES}
        ${CRYPTOPP_LIBRARIES}
        log4cplus
        concord_utils_shared
        bftclient_shared
        bftclient_new
        util_shared
        )
target_include_directories(concord_external_client_shared PUBLIC ../../submodules/concord-bft/logging/include)
target_include_directories(concord_external_client_shared PUBLIC ../../submodules/concord-bft/logging/include)
target_include_directories(concord_external_client_shared PUBLIC ../../submodules/concord-bft/util/include)
target_include_directories(concord_external_client_shared PUBLIC ../../submodules/concord-bft/communication/include/communication)
target_include_directories(concord_external_client_shared PUBLIC ../../submodules/concord-bft/bftclient/include/bftclient)
target_include_directories(concord_external_client_shared PUBLIC ../../submodules/concord-bft/threshsign/src)
target_include_directories(concord_external_client_shared PUBLIC ../../submodules/concord-bft/storage/include/)

# Hack for including SysConsts.hpp
target_include_directories(concord_external_client_shared PUBLIC ../../submodules/concord-bft/bftengine/src/bftengine)
target_include_directories(concord_external_client_shared PUBLIC ../../submodules/concord-bft/bftengine/include/bftengine)

install (TARGETS concord_external_client_shared DESTINATION lib${LIB_SUFFIX})

configure_file(extclient-wrapper-entrypoint.sh extclient-wrapper-entrypoint.sh COPYONLY)
