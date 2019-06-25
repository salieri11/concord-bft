// Copyright 2019 VMware, all rights reserved
//
// Update and/or read the time contract.

#include <inttypes.h>
#include <boost/program_options.hpp>
#include <chrono>
#include <iomanip>
#include <iostream>

#include "concmdconn.hpp"
#include "concmdex.hpp"
#include "concmdfmt.hpp"
#include "concmdopt.hpp"
#include "concord.pb.h"

using namespace boost::program_options;
using namespace com::vmware::concord;
using std::chrono::system_clock;

#define OPT_SOURCE "source"
#define OPT_TIME "time"
#define OPT_GET "get"
#define OPT_LIST "list"

void add_options(options_description &desc) {
  // clang-format off
  desc.add_options()
    (OPT_SOURCE ",s", value<std::string>(), "Source of the update sample")
    (OPT_TIME ",t", value<uint64_t>(), "Time of the update sample")
    (OPT_GET ",g", bool_switch()->default_value(false),
     "Fetch the accumulated time")
    (OPT_LIST ",l", bool_switch()->default_value(false),
     "Fetch all stored samples");
  // clang-format on
}

int main(int argc, char **argv) {
  try {
    variables_map opts;
    if (!parse_options(argc, argv, &add_options, opts)) {
      return 0;
    }

    // Create request

    ConcordRequest concReq;
    TimeRequest *timeReq = concReq.mutable_time_request();

    // Allow sending source name without time, or time without name, for testing
    // purposes.
    if (opts.count(OPT_SOURCE) > 0 || opts.count(OPT_TIME) > 0) {
      TimeSample *sample = timeReq->mutable_sample();
      if (opts.count(OPT_SOURCE) > 0) {
        sample->set_source(opts[OPT_SOURCE].as<std::string>());
      }
      if (opts.count(OPT_TIME) > 0) {
        sample->set_time(opts[OPT_TIME].as<uint64_t>());
      }
    }
    if (opts[OPT_GET].as<bool>()) {
      timeReq->set_return_summary(true);
    }
    if (opts[OPT_LIST].as<bool>()) {
      timeReq->set_return_samples(true);
    }

    if (opts.count(OPT_SOURCE) == 0 && opts.count(OPT_TIME) == 0 &&
        !(opts[OPT_GET].as<bool>() || opts[OPT_LIST].as<bool>())) {
      // if no options are specified, default to just "get"
      timeReq->set_return_summary(true);
    }

    // Send & Receive

    ConcordResponse concResp;
    if (call_concord(opts, concReq, concResp)) {
      if (concResp.has_time_response()) {
        TimeResponse tr = concResp.time_response();
        if (tr.has_summary()) {
          std::chrono::milliseconds summary_ms(tr.summary());
          system_clock::time_point summary_tp(summary_ms);
          std::time_t summary_tt = system_clock::to_time_t(summary_tp);
          std::tm *t = std::gmtime(&summary_tt);
          std::cout << "The current time is: " << std::put_time(t, "%FT%T")
                    << "." << (summary_ms % std::chrono::seconds(1)).count()
                    << "Z (" << tr.summary() << ")" << std::endl;
        }

        if (tr.sample_size() > 0) {
          for (int i = 0; i < tr.sample_size(); i++) {
            TimeSample ts = tr.sample(i);
            std::cout << "Sample " << (i + 1) << " from \'"
                      << (ts.has_source() ? ts.source() : "[unknown]")
                      << "\' read "
                      << (ts.has_time() ? std::to_string(ts.time())
                                        : "[unknown]")
                      << std::endl;
          }
        }
      } else if (concResp.error_response_size() == 1) {
        ErrorResponse errorResp = concResp.error_response(0);
        if (errorResp.has_description()) {
          std::cout << "Error Response: " << errorResp.description()
                    << std::endl;
          return -1;
        } else {
          std::cout << "Error response had no description" << std::endl;
          return -1;
        }
      } else {
        std::cerr << "No time_response found, and wrong number of  errors ("
                  << concResp.error_response_size() << ")"
                  << " (expected 1)" << std::endl;
        return -1;
      }
    } else {
      return -1;
    }
  } catch (std::exception &e) {
    std::cerr << "Exception: " << e.what() << std::endl;
    return -1;
  }

  return 0;
}
