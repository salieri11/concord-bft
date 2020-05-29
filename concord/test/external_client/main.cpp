#include "external_client_pool.h"

#include <iostream>

const int DEFAULT_PORT = 8989;

int main(int argc, char* argv[]) {
  if (argc != 2) {
    std::cerr << "Usage: " << argv[0] << " <path to config file>\n";
    return 1;
  }

  ExternalClient cl(argv[1], DEFAULT_PORT);
  cl.Start();
}