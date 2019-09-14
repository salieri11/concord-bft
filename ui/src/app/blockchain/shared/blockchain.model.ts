/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

export class BlockchainRequestParams {
  consortium_id?: string;
  consortium_name: string;
  f_count: number;
  c_count: number = 0;
  deployment_type: string = 'FIXED';
  zone_ids: string[];
  blockchain_type: string;
}

export interface Node {
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
  node_list: Node[];
}

export interface Zone {
  id: string;
  name: string;
  latitude: number;
  longitude: number;
  password?: string;
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

export const fakeZones: Zone[] = [{
  name: 'US West - Oregon',
  id: 'us-west',
  latitude: 0,
  longitude: 0
}, {
  name: 'US East - N Virginia',
  id: 'us-east',
  latitude: 0,
  longitude: 0
}, {
  name: 'EMEA - Frankfurt',
  id: 'emea',
  latitude: 0,
  longitude: 0
},
{
  name: 'Pacific - Sydney',
  id: 'pacific',
  latitude: 0,
  longitude: 0
}];
