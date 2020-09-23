# Generate CPP header from a CMF file
#
# cmf_generate_cpp(<LIST_OF_GENERATED_HEADER_FILES> <DEST_DIR> <CMFs> ...)
#
# LIST_OF_GENERATED_HEADER_FILES - Will be populated with all generated files
# DEST_DIR - Hopefully a location inside a build directory
# CMFs - List of CMF files
function(CMF_GENERATE_CPP CPP_HEADER CPP_IMPL)
  foreach(FIL ${ARGN})
    if(NOT EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${FIL}")
      message(FATAL_ERROR "CMF doesn't exist: ${CMAKE_CURRENT_SOURCE_DIR}/${FIL}")
    endif()
    set(IN_FILE_ABS "${CMAKE_CURRENT_SOURCE_DIR}/${FIL}")
    add_custom_command(
      OUTPUT "${FIL}.hpp" "${FIL}.cpp"
      COMMAND ${CMF_COMPILER}
      ARGS --input ${IN_FILE_ABS}
           --output ${FIL}
           --language cpp
           --namespace concord::messages
      DEPENDS ${CMF_COMPILER}
      COMMENT "CMFC: Generate C++ code for ${FIL}"
      VERBATIM
    )
    list(APPEND CPP_HEADER "${FIL}.hpp")
    list(APPEND CPP_IMPL "${FIL}.cpp")
  endforeach()
  set_source_files_properties(${CPP_HEADER} PROPERTIES GENERATED TRUE)
  set(${CPP_HEADER} ${${CPP_HEADER}} PARENT_SCOPE)
  set_source_files_properties(${CPP_IMPL} PROPERTIES GENERATED TRUE)
  set(${CPP_IMPL} ${${CPP_IMPL}} PARENT_SCOPE)
endfunction()

# Find CMFC
find_program(CMF_COMPILER cmfc.py
  HINTS "../concord/submodules/concord-bft/messages/compiler/"
)
if(NOT CMF_COMPILER)
  message(FATAL_ERROR "Couldn't find CMF compiler")
endif()
message(STATUS "cmfc.py found at ${CMF_COMPILER}")
