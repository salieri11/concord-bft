/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Zone } from '../../zones/shared/zones.model';

export class BlockchainRequestParams {
  consortium_id?: string;
  consortium_name: string;
  replica_zone_ids: string[];
  blockchain_type: string;
  client_nodes: NodeClientParam[];
}

export interface NodeCommitter {
  cert: string;
  ip: string;
  node_id: string;
  zone_id: string;
  region: string;
  url: string;
}

export interface NodeClientParam {
  zone_id: string;
  auth_url_jwt: string;
}

export interface BlockchainResponse {
  id: string;
  consortium_id: string;
  consortium_name?: string;
  created: number;
  created_by: string;
  version: string;
  blockchain_type: string;
  blockchain_state: string;
}

export interface BlockchainMeta {
  consortium_id: string;
  id: string;
  blockchain_type: ContractEngines;
  node_list: {
    cert: string,
    ip: string,
    node_id: string,
    url: string,
    zone_id: string,
    zone?: Zone
  }[];
}

export enum DeployStates {
  NONE = 'NONE',
  RUNNING = 'RUNNING',
  SUCCEEDED = 'SUCCEEDED',
  FAILED = 'FAILED',
}

export enum ContractEngines {
  DAML = 'DAML',
  ETH = 'ETHEREUM',
  HLF = 'HLF'
}
