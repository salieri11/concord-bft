/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { ZoneType } from '../../blockchain/shared/blockchain.model';

/**
 * GET response of fetching a list of members
 */
export interface NodeInfo {
  hostname: string;
  status: string;
  address: string;
  millis_since_last_message: number;
  millis_since_last_message_threshold: number;
  health?: boolean;
  healthy?: boolean;
  healthHTML?: string;
  id?: string;
  organization?: string;
  location?: string;
  geo?: [number, number]; // long, lat
  zone_id?: string;
  zone_type?: ZoneType;
}

export enum GeoCoordinate {
  longitude = 0,
  latitude = 1,
}

export interface NodeProperties {
  location: string;
  geo?: [number, number]; // long, lat
  nodes: NodeInfo[];
  type?: string;
}

export interface NodesResponse {
  nodes: NodeInfo[];
  nodesByLocation: NodeProperties[];
  onlyOnPrem: boolean;
  zone_id?: string;
}
