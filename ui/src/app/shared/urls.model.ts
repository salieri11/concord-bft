/*
 * Copyright 2019 VMware, all rights reserved.
 */

export enum CspAPIs {
  orgs = '/csp/gateway/am/api/orgs/',
  refLink = '/csp/gateway/slc/api/definitions/external/'
}

export enum QueryParams {
  userKey = 'user',
  userNewValue = 'new'
}

export enum External {
  docs = 'https://docs-staging.vmware.com/en/VMware-Blockchain/1.0/getting_started/GUID-1C20DB34-8195-433C-AFE1-59F6EA874560.html'
}

export enum ConsortiumStates {
  deploying = 'deploying',
  loginReturn = 'login-return'
}

export enum FeatureFlagSource {
  URL = 'static/feature-flag.json'
}

export const mainRoutes = {
  forbidden: 'forbidden',
  error: 'error',
  // Blockchain child routes
  dashboard: 'dashboard',
  blocks: 'blocks',
  nodes: 'nodes',
  smartContracts: 'smart-contracts',
  logging: 'logging',
  consortiums: 'consortiums',
  organizations: 'organizations',
  users: 'users',
  transactions: 'transactions',
  developer: 'developer'
};

export const FeatureFlagRouteMapping = {
  nodes: 'node_list',
  logging: 'developer_logging'
};

export const Apis = {
  base: 'api',
  blockchainsApi: 'blockchains',

  get blockchains() {
    return `${this.base}/${this.blockchainsApi}`;
  },

  get zones() {
    return `${this.blockchains}/zones`;
  },

  get zonesReload() {
    return `${this.zones}?action=reload`;
  },

  get zonesTextConnection() {
    return `${this.zones}?action=test`;
  },

  getReplicas(id: string) {
    return `${this.blockchains}/${id}/replicas`;
  },
};
