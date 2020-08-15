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
  host_name?: string;
  name?: string;
  private_ip: string;
  zone?: Zone;
  zone_id?: string;
  zone_type?: ZoneType;
  zone_name?: string;
  node_type?: NodeType;
  strong_password?: string;
  auth_url_jwt?: string;
  cert?: string;
  group_id?: string;
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

export interface TemplateItem {
  title?: string;
  type?: string;
  no_of_cpus: any;
  storage_in_gigs: any;
  memory_in_gigs:  any;
  icon?: string;
  description?: string;
}

export interface NodeTemplate {
  name?: string;
  items: TemplateItem[];
}

export interface NodeTemplates {
  id: string;
  name: string;
  templates: NodeTemplate[];
  range: TemplateItem;
}
export interface NodeTemplateFormResponse {
  committerSizing: TemplateItem;
  clientSizing?: TemplateItem;
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


export const nodeSizingOptionsBase = {
  'id': '<UUID>',
  'name': 'nodeSizeTemplate',
  'templates': [
    {
      'name': 'Small',
      'items': [
        {
          'type': 'committer',
          'no_of_cpus': '4',
          'storage_in_gigs': '1024',
          'memory_in_gigs': '32'
        },
        {
          'type': 'client',
          'no_of_cpus': '4',
          'storage_in_gigs': '1024',
          'memory_in_gigs': '32'
        }
      ],
    },
    {
      'name': 'Medium',
      'items': [
        {
          'type': 'committer',
          'no_of_cpus': '8',
          'storage_in_gigs': '1024',
          'memory_in_gigs': '32'
        },
        {
          'type': 'client',
          'no_of_cpus': '8',
          'storage_in_gigs': '1024',
          'memory_in_gigs': '32'
        }
      ]
    },
    {
      'name': 'Large',
      'items': [
        {
          'type': 'committer',
          'no_of_cpus': '16',
          'storage_in_gigs': '1024',
          'memory_in_gigs': '64'
        },
        {
          'type': 'client',
          'no_of_cpus': '16',
          'storage_in_gigs': '1024',
          'memory_in_gigs': '64'
        }
      ]
    }
  ],
  'range': {
    'no_of_cpus': {'min': 1, 'max': 18 },
    'storage_in_gigs': {'min': 1, 'max': 16384 },
    'memory_in_gigs': {'min': 1, 'max': 3024 }
  }
};


export const mockCommitters: NodeInfo[] = [
  {
    name: 'Committer0',
    id: '0aaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',
    public_ip: '0.0.0.0', private_ip: '0.0.0.0', rpc_url: '',
    status: 'live', certificate: null,
    millis_since_last_message: 0, millis_since_last_message_threshold: 1,
    zone_id: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa'
  },
  {
    name: 'Committer1',
    id: '1aaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',
    public_ip: '0.0.0.0', private_ip: '0.0.0.0', rpc_url: '',
    status: 'live', certificate: null,
    millis_since_last_message: 0, millis_since_last_message_threshold: 1,
    zone_id: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa'
  },
  {
    name: 'Committer2',
    id: '2aaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',
    public_ip: '0.0.0.0', private_ip: '0.0.0.0', rpc_url: '',
    status: 'live', certificate: null,
    millis_since_last_message: 0, millis_since_last_message_threshold: 1,
    zone_id: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa'
  },
  {
    name: 'Committer3',
    id: '3aaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',
    public_ip: '0.0.0.0', private_ip: '0.0.0.0', rpc_url: '',
    status: 'live', certificate: null,
    millis_since_last_message: 0, millis_since_last_message_threshold: 1,
    zone_id: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa'
  },
];

export const mockClients: ClientNode[] = [
  {
    id: 'a0aaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',
    public_ip: '0.0.0.0', private_ip: '0.0.0.0', name: null,
    url: 'https://0.0.0.0:6865',
    auth_url_jwt: null, cert: null,
    group_id: '0aaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa', // Group 0
    zone_id: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa'
  },
  {
    id: 'a1aaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',
    public_ip: '0.0.0.0', private_ip: '0.0.0.0', name: null,
    url: 'https://0.0.0.0:6865',
    auth_url_jwt: null, cert: null,
    group_id: '1aaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa', // Group 1
    zone_id: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa'
  },
  {
    id: 'a2aaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',
    public_ip: '0.0.0.0', private_ip: '0.0.0.0', name: null,
    url: 'https://0.0.0.0:6865',
    auth_url_jwt: null, cert: null,
    group_id: '1aaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa', // Group 1
    zone_id: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa'
  }
];

export const mockNodeCredentials = {
  username: 'root',
  password: 'mockpw' // dummy; not a security concern
};
