/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */
import { ZoneType, Zone } from '../../zones/shared/zones.model';

/**
 * GET response of fetching a list of members
 */
export interface NodeInfo {
  certificate?: string;
  geo?: [number, number]; // long, lat
  healthy?: boolean;
  healthHTML?: string;
  id?: string;
  location?: string;
  millis_since_last_message: number;
  millis_since_last_message_threshold: number;
  name?: string;
  name_ordinal?: string;
  private_ip?: string;
  public_ip?: string;
  rpc_url?: string;
  state?: string;
  status?: string;
  zone?: Zone;
  zone_id?: string;
  zone_type?: ZoneType;
  zone_name?: string;
  node_type?: NodeType;
  strong_password?: string;
}

export interface ClientNode {
  id: string;
  name_ordinal?: string;
  url: string;
  public_ip: string;
  host_name: string;
  name?: string;
  private_ip: string;
  zone?: Zone;
  zone_id?: string;
  zone_type?: ZoneType;
  zone_name?: string;
  node_type?: NodeType;
  strong_password?: string;
}

export type BlockchainNode = NodeInfo // Committer
                            | ClientNode; // Client

export interface ClientNodeDeployParams {
  name?: string;
  high_availability?: boolean;
  zone_ids: string[];
  client_jwt: string;
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

export enum NodeType {
  committers = 'committers',
  clients = 'clients',
}

export interface CommittersData {
  nodes: NodeInfo[];
  nodesByLocation: NodeProperties[];
  onlyOnPrem: boolean;
}

export interface NodeCredentials {
  username: string;
  password: string;
}
