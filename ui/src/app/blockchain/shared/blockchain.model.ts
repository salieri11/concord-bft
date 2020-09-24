/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { NodeClientParam, TemplateItem } from '../../nodes/shared/nodes.model';


export class BlockchainRequestParams {
  consortium_id?: string;
  consortium_name: string;
  replica_zone_ids: string[];
  replica_nodes: {zone_id: string, sizing_info: TemplateItem}[];
  blockchain_type: string;
  client_nodes: NodeClientParam[];
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

export const mockBlockchains = [{
  id: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',
  consortium_id: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',
  blockchain_type: 'ETHEREUM',
  blockchain_state: 'ACTIVE',
  version: 'Blockchain Version: NA',
  created_by: 'Helen',
  created: 1596437339711
}, {
  id: 'baaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',
  consortium_id: 'baaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',
  blockchain_type: 'DAML',
  blockchain_state: 'ACTIVE',
  version: 'Blockchain Version: stable, DAML SDK Version: 1.3.0-snapshot.20200610.4413.0.11b5c362',
  created_by: 'Helen',
  created: 1596437339711
}, {
  id: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',
  consortium_id: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',
  blockchain_type: 'ETHEREUM',
  blockchain_state: 'FAILED', // FAILED should not display
  version: 'Blockchain Version: NA',
  created_by: 'Helen',
  created: 1596437339711
}];
