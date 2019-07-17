#!/bin/sh

# Script to create gRPC python bindings from source protobuf and
# convert the bindings suitable for python3

GRPC_PYTHON_DEST_DIR=hermes/lib/persephone/grpc_python_bindings
while [ "$1" != "" ] ; do
   case $1 in
      "--protobufSrc")
         shift
         PROTOBUF_SRC_DIR="$1"
         ;;
      "--grpcPythonDir")
         shift
         GRPC_PYTHON_DEST_DIR="$1"
         ;;
   esac
   shift
done

generate_py_grpc_bindings() {
    PROTOBUF_SRC_DIR="$1"
    GRPC_PYTHON_DEST_DIR="$2"
    if [ ! -d "${PROTOBUF_SRC_DIR}" ]
    then
        echo "ERROR: protobuf source directory does not exist: ${PROTOBUF_SRC_DIR}"
        exit 1
    else
        if [ ! -d "${GRPC_PYTHON_DEST_DIR}" ]
        then
            mkdir -p "${GRPC_PYTHON_DEST_DIR}"
        fi
        echo "Generating python bindings..."
        if [ -z "${python}" ]
        then
            python=/usr/local/bin/python3
        fi
        "${python}" -m grpc_tools.protoc -I="${PROTOBUF_SRC_DIR}" --python_out="${GRPC_PYTHON_DEST_DIR}" --grpc_python_out="${GRPC_PYTHON_DEST_DIR}" "${PROTOBUF_SRC_DIR}"/*.proto
        RC=$?
        if [ "${RC}" -ne "0" ]
        then
            echo "Creation of python gRPC bindings failed."
            echo "Possible reason: grpcio & grpcio-tools are not installed (pip3 install grpcio grpcio-tools)"
            exit 1
        fi
    fi
}

support_py_bindings_python3() {
    GRPC_PYTHON_DEST_DIR="$1"
    if [ ! -d "${GRPC_PYTHON_DEST_DIR}" ]
    then
        echo "ERROR: python bindings not found: ${GRPC_PYTHON_DEST_DIR}"
        exit 1
    fi

    echo "Converting python bindings to support python3..."
    cp -r "${GRPC_PYTHON_DEST_DIR}" "${GRPC_PYTHON_DEST_DIR}.orig"
    tmp_file=/tmp/tmp_grpc_py_binding.txt
    if [ -f "${tmp_file}" ]
    then
        rm -f "${tmp_file}"
    fi
    for filename in `ls "${GRPC_PYTHON_DEST_DIR}"/*.py`
    do
        module_name=`basename "${filename}" | cut -d '.' -f1`
        find_string="import ${module_name} as "
        echo "${find_string}" >> "${tmp_file}"
    done

    while read -r line
    do
        SEARCH_STR="${line}"
        echo "  ${SEARCH_STR}..."
        REPLACE_STR="from . ${SEARCH_STR}"
        sed -i.bkp "s/^${SEARCH_STR}/${REPLACE_STR}/g" "${GRPC_PYTHON_DEST_DIR}"/*py
    done < "${tmp_file}"
}

if [ -z "${GRPC_PYTHON_DEST_DIR}" -o -z "${PROTOBUF_SRC_DIR}" ]
then
    echo "Usage: $0 [Options]"
    echo "Options:"
    echo "\t--protobufSrc <protobuf source dir>"
    echo "Optional:"
    echo "\t[--grpcPythonDir <gRPC python destination dir. default: ${GRPC_PYTHON_DEST_DIR}>]"
    echo "\nExample: $0 --protobufSrc persephone/api/src/protobuf [--grpcPythonDir hermes/lib/persephone/grpc_py_bindings]"
    exit 1
fi

generate_py_grpc_bindings "${PROTOBUF_SRC_DIR}" "${GRPC_PYTHON_DEST_DIR}"
support_py_bindings_python3 "${GRPC_PYTHON_DEST_DIR}"

exit 0
