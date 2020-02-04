/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { DeployStates } from './../../blockchain/shared/blockchain.model';

export interface Task {
  task_id?: string;
  state?: DeployStates;
  message?: string;
  resource_id?: string;
  resource_link?: any;
}

export interface TaskResponse {
   task_id: string;
}
