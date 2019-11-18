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

export interface Zone {
  type: ZoneType;
  id: string;
  name: string;
  latitude?: number;
  longitude?: number;
  password?: string;
}

export interface OnPremZone extends Zone {
  folder: string;
  resource_pool: string;
  storage: string;
  vcenter: OnPremVCenter;
  container_repo: OnPremContainerRepo;
  network: OnPremNetwork;
}

export interface OnPremVCenter {
  password: string;
  url: string;
  username: string;
}

export interface OnPremContainerRepo {
  password: string;
  url: string;
  username: string;
}

export interface OnPremNetwork {
  gateway: string;
  name_servers?: [];
  ip_pool: [];
  name: string;
  subnet: string;
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

export enum ZoneType {
  NONE = 'NONE',
  VMC_AWS = 'VMC_AWS',
  ON_PREM = 'ON_PREM'
}

export const fakeZones: Zone[] = [{
  name: 'US West - Oregon',
  id: 'us-west',
  latitude: 0,
  longitude: 0,
  type: ZoneType.VMC_AWS
}, {
  name: 'US East - N Virginia',
  id: 'us-east',
  latitude: 0,
  longitude: 0,
  type: ZoneType.VMC_AWS
}, {
  name: 'EMEA - Frankfurt',
  id: 'emea',
  latitude: 0,
  longitude: 0,
  type: ZoneType.VMC_AWS
},
{
  name: 'Pacific - Sydney',
  id: 'pacific',
  latitude: 0,
  longitude: 0,
  type: ZoneType.VMC_AWS
}];
