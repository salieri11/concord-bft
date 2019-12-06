/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Zone } from '../../zones/shared/zones.model';

export class BlockchainRequestParams {
  consortium_id?: string;
  consortium_name: string;
  f_count: number;
  c_count: number = 0;
  deployment_type: string = 'FIXED';
  zone_ids: string[];
  blockchain_type: string;
}

export interface NodeCommitter {
  cert: string;
  ip: string;
  node_id: string;
  region: string;
  url: string;
}

export interface BlockchainResponse {
  id: string;
  consortium_id: string;
  consortium_name: string;
  node_list: NodeCommitter[];
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
