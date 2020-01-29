/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

export interface Task {
  task_id?: string;
  state?: 'RUNNING' | 'SUCCEEDED' | 'FAILED';
  message?: string;
  resource_id?: string;
  resource_link?: any;
}

export interface TaskResponse {
   task_id: string;
}
