/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

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
  group_index: string;
}

export enum BlockchainStates {
  // As specified in api.yaml (/blockchains)
  ACTIVE = 'ACTIVE',
  FAILED = 'FAILED',
  INACTIVE = 'INACTIVE',
}

export interface BlockchainResponse {
  id: string;
  consortium_id: string;
  consortium_name?: string;
  created: number;
  created_by: string;
  version: string;
  blockchain_type: ContractEngines;
  blockchain_state: BlockchainStates;
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

// Deploying data
export interface TempDeployTracker {
  key: string;
  create_params: BlockchainRequestParams;
  requested: number;
  responded?: number;
  responded_task?: number;
  consortium_id?: string;
  consortium_name?: string;
  organization_id?: string;
  task_id?: string;
  state?: string; // DeployStates, NONE | RUNNING | SUCCEEDED | FAILED
  stage_name?: string; // Deployment stage name
  stage?: number; // Stage number
  at?: number; // Last known percentage for interstitial
}

export interface TempDeployTrackerRegistry {
  [key: string]: TempDeployTracker;
}
