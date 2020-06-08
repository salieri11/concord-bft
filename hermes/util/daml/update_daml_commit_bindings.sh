#!/bin/bash

# Overly cautious saftey check: Make sure you're in the right directory
if [[ $PWD != *daml ]]; then
  >&2 echo "Error: Make sure you're in hermes/util/daml"
  exit 1;
fi

# Tested/Run with Python3
python \
  -m grpc_tools.protoc \
  -I $PWD/../../../concord/proto/ \
  --python_out=. \
  --grpc_python_out=. \
  $PWD/../../../concord/proto/daml_commit.proto