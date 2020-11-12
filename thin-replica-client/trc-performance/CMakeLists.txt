# Copyright 2020 VMware, all rights reserved

cmake_minimum_required (VERSION 3.10)

add_executable(trc_performance trc_performance.cpp)
target_link_libraries(trc_performance PUBLIC
  thin_replica_client
  log4cplus
)
