#!/bin/bash

# Overly cautious saftey check: Make sure you're in the right directory
if [[ $PWD != *thin_replica ]]; then
  >&2 echo "Error: Make sure you're in hermes/util/thin_replica"
  exit 1;
fi

# Tested/Run with Python3
python \
  -m grpc_tools.protoc \
  -I $PWD/../../../communication/src/main/proto/ \
  --python_out=. \
  --grpc_python_out=. \
  $PWD/../../../communication/src/main/proto/thin_replica.proto
