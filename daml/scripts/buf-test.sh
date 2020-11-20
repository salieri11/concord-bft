#!/usr/bin/env bash

set -e
set -u
set -o pipefail

BUF_CONFIG='{"version": "v1beta1", "build": {"roots": ["concord/proto", "communication/src/main/proto"]}}'

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." >/dev/null 2>&1 && pwd)"
DAML_DIR="${ROOT_DIR}/daml"
DAML_SDK_VERSION="$(cat "${DAML_DIR}/sdk.version")"
DAML_INTEGRATION_KIT_VERSION="${DAML_SDK_VERSION}-$(cat "${DAML_DIR}/integration-kit-suffix.version")"
EXECUTION_ENGINE_GRPC_JAR="${DAML_DIR}/execution-engine/target/universal/stage/lib/com.digitalasset.daml.execution.engine.execution-engine-grpc-${DAML_INTEGRATION_KIT_VERSION}.jar"

PROTO_DIR="$(mktemp -d)"
trap 'rm -rf "$PROTO_DIR"' EXIT

cd "$PROTO_DIR"
unzip "$EXECUTION_ENGINE_GRPC_JAR" '*.proto'
echo
echo 'DAML proto files:'
ls -lh

cd "$ROOT_DIR"
echo
echo 'Buf configuration:'
echo "${BUF_CONFIG}"
echo
echo 'Running `buf check breaking`...'
buf --config="$BUF_CONFIG" check breaking --against="$PROTO_DIR"
echo 'Check succeeded.'
