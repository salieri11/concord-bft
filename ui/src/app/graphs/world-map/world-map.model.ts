/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

export interface NodeProperties {
  location: string;
  nodes: [{
    id: string,
    consortiumName: string;
    blockchainType: string;
    status: string;
  }];
}
