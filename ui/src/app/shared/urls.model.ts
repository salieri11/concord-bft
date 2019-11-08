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
  waiting = 'waiting', // waiting for taskId
  loginReturn = 'login-return'
}

export enum FeatureFlagSource {
  URL = 'static/feature-flag.json'
}

export const uuidRegExp
  = /^[0-9A-F]{8}-[0-9A-F]{4}-4[0-9A-F]{3}-[89AB][0-9A-F]{3}-[0-9A-F]{12}$/i;

export const authRoutes = {
  base: 'auth',
  onboarding: 'onboarding', // used by TOS agreement
  login: 'login', // ! no longer used, deprecated by CSP, only used by `npm test`
  signup: 'signup', // ! no longer used, deprecated by CSP, only used by `npm test`

  // Fleeting, part of temporary redirect flow
  loginReturn: 'login-return'
};

export const mainRoutes = {
  // General
  forbidden: 'forbidden',
  error: 'error',
  get baseAllowed() { return [ // allowed 1st level dir, base path other than uuidv4
    authRoutes.base, this.forbidden, this.error
  ]; },

  // Resolved Consortium ID and child routes
  dashboard: 'dashboard',
  blocks: 'blocks',
  nodes: 'nodes',
  smartContracts: 'smart-contracts',
  logging: 'logging',
  consortiums: 'consortiums',
  organizations: 'organizations',
  users: 'users',
  transactions: 'transactions',
  developer: 'developer',
  get consortiumIdChildren() { return [ // allowed paths under /:consortiumId/*
    this.dashboard, this.blocks, this.nodes, this.smartContracts, this.logging,
    this.consortiums, this.organizations, this.users, this.transactions, this.developer
  ]; },

  // Path of /blockchain/*
  blockchain: 'blockchain',
  welcome: 'welcome',
  deploy: 'deploy',
  deploying: 'deploying',
  get blockchainChildren() { return [ // allowed paths under /blockchain/*
    this.welcome, this.deploy, this.deploying
  ]; },

  // Deploying flow URLs
  get deployingBaseRoute() { return [mainRoutes.blockchain, mainRoutes.deploying]; },
  get deployingWaitingURL() { return this.deployingBaseRoute.concat([ConsortiumStates.waiting]).join('/'); },

};

export const mainFragments = {
  welcome: 'welcome',
  defaultTour: 'orgTour',
};

// routes that cannot be destinations (only part of redirect flow)
export const fleetingRoutesList = [
  authRoutes.loginReturn,
  mainRoutes.deployingWaitingURL,
];

export const FeatureFlagRouteMapping = {
  nodes: 'node_list',
  logging: 'developer_logging'
};

export const Apis = {
  base: 'api',
  blockchainsApi: 'blockchains',

  get blockchains() { return `${this.base}/${this.blockchainsApi}`; },

  get tasks() { return `${this.base}/tasks`; },

  get zones() { return `${this.blockchains}/zones`; },

  get zonesReload() { return `${this.zones}?action=reload`; },

  get zonesTestConnection() { return `${this.zones}?action=test`; },

  getReplicas(id: string) { return `${this.blockchains}/${id}/replicas`; },
};
