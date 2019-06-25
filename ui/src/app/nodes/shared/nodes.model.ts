/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

/**
 * GET response of fetching a list of members
 */
export interface Node {
  hostname: string;
  status: string;
  address: string;
  millis_since_last_message: number;
  millis_since_last_message_threshold: number;
  health?: boolean;
  healthHTML?: string;
  id?: string;
  organization?: string;
  location?: string;
  geo?: string;
}

export interface NodeProperties {
  location: string;
  geo: string;
  nodes: Node[];
}

export interface NodesResponse {
  nodes: Node[];
  nodesByLocation: NodeProperties[];
}
