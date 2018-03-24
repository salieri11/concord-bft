find_path(Cryptopp_INCLUDE_DIR keccak.h
          HINTS /usr/local/include/cryptopp
                ${CMAKE_CURRENT_SOURCE_DIR}/../cryptopp)

if(Cryptopp_INCLUDE_DIR EQUAL Cryptopp_INCLUDE_DIR-NOTFOUND)
   message(SEND_ERROR "Cryptopp not found")
else()
   message(STATUS "Cryptopp found at " ${Cryptopp_INCLUDE_DIR})
   set(Cryptopp_INCLUDE_DIRS ${Cryptopp_INCLUDE_DIR})

   find_path(Cryptopp_LIB_DIR libcryptopp.a
             HINTS ${Cryptopp_INCLUDE_DIR}../lib/
             ${Cryptopp_INCLDUE_DIR}/build/)
   if(Cryptopp_LIB_DIR_NOTFOUND EQUAL Cryptopp_INCLUDE_DIR-NOTFOUND)
      message(SEND_ERROR "Cryptopp lib not found")
   else()
      link_directories(${Cryptopp_LIB_DIR})
      set(Cryptopp_LIBRARIES cryptopp)

      set(Cryptopp_FOUND 1)
   endif(Cryptopp_LIB_DIR_NOTFOUND EQUAL Cryptopp_INCLUDE_DIR-NOTFOUND)
endif(Cryptopp_INCLUDE_DIR EQUAL Cryptopp_INCLUDE_DIR-NOTFOUND)
