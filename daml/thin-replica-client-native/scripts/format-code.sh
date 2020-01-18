#!/usr/bin/env sh

SOURCE_DIRECTORY="thin-replica-client-native"

if [ -z "$1" ]; then
  >&2 echo "ERROR: Path to ${SOURCE_DIRECTORY} directory required"
  exit 1
fi

# Construct the absolute path
export TMP_SOURCE_DIR="$1"
ABS_SOURCE_PATH=$(
  python -c 'import os; print(os.path.abspath(os.environ["TMP_SOURCE_DIR"]))')
unset TMP_SOURCE_DIR

# Overly cautious saftey check
IS_EXPECTED_NAME=$(echo ${ABS_SOURCE_PATH} | grep "${SOURCE_DIRECTORY}$")

if [ ! -d ${ABS_SOURCE_PATH} ] || [ -z ${IS_EXPECTED_NAME} ]; then
  >&2 echo \
    "ERROR: Couldn't find ${SOURCE_DIRECTORY} directory \"${ABS_SOURCE_PATH}\""
  exit 1;
fi

FILES_TO_FORMAT=$(find ${ABS_SOURCE_PATH} \
  -type f \( \
    -iname "*.c" -o \
    -iname "*.cc" -o \
    -iname "*.cpp" -o \
    -iname "*.h" -o \
    -iname "*.hpp" \) \
  -a -not -path "${ABS_SOURCE_PATH}/*/include/*" \
  -a -not -path "${ABS_SOURCE_PATH}/build/*" \
  -a -not -path "${ABS_SOURCE_PATH}/target/*" )

echo "Formatting files:\n${FILES_TO_FORMAT}"

if [ -n "$2" ]; then
  if [ "$2" = "--is-required" ]; then
    NUM_CHANGES=$(clang-format \
      -style=file \
      -fallback-style=none \
      -output-replacements-xml ${FILES_TO_FORMAT} \
      | grep "<replacement offset" \
      | wc -l)
    if [ ${NUM_CHANGES} -ne 0 ]; then
      # Note: exit_code = return_value % 255
      echo "Code format changes needed"
      exit 1
    else
      echo "No format changes needed"
      exit 0
    fi
  fi
  >&2 echo "ERROR: Unknown parameter \"$2\""
  exit 1
else
  clang-format -style=file -fallback-style=none -i ${FILES_TO_FORMAT}
fi
