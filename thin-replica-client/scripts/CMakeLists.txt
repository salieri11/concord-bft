find_program(
  CLANG_FORMAT_EXE
  clang-format
  DOC "Path to clang-format"
)
find_program(
  THIN_REPLICA_CLIENT_FORMAT_CODE_SCRIPT
  format-code.sh
  HINTS ../scripts/
  NO_DEFAULT_PATH
  DOC "Path to format-code.sh"
)
if(CLANG_FORMAT_EXE)
  if(THIN_REPLICA_CLIENT_FORMAT_CODE_SCRIPT)
    add_custom_target(format-check ALL
      COMMENT "Check code format via ${THIN_REPLICA_CLIENT_FORMAT_CODE_SCRIPT}"
      COMMAND ${THIN_REPLICA_CLIENT_FORMAT_CODE_SCRIPT}
              ${CMAKE_CURRENT_SOURCE_DIR}/.. --is-required)
    add_custom_target(format
      COMMENT "Change code format via ${THIN_REPLICA_CLIENT_FORMAT_CODE_SCRIPT}"
      COMMAND ${THIN_REPLICA_CLIENT_FORMAT_CODE_SCRIPT}
              ${CMAKE_CURRENT_SOURCE_DIR}/..)
  else()
    message("Couldn't find format-code.sh")
  endif()
else()
  message("Couldn't find clang-format")
endif()
