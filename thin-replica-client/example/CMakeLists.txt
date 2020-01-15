# Copyright 2019 VMware, all rights reserved

cmake_minimum_required(VERSION 3.10)

add_executable(trc_example main.cpp)
target_link_libraries(trc_example
  thin_replica_client
  log4cplus
)
