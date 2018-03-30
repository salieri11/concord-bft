// Copyright 2018 VMware, all rights reserved
//
// Formatting utilities for command line tools.

#ifndef ATHCMDFMT_HPP
#define ATHCMDFMT_HPP

char hexval(char c);
void dehex0x(const std::string &str, std::string &bin /* out */);

char hexchar(char c);
void hex0x(const std::string &in, std::string &out /* out */);

#endif
