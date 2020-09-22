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

export const hexAddress160RegExp = /^0x[a-fA-F0-9]{40}$/; // hex, eth address 160-bit
export const hexHash256HexRegExp = /^0x[a-fA-F0-9]{64}$/; // hex, tx hash, block hash, 256-bit

export const uuidRegExp // strict UUIDv4
  = /^[0-9A-F]{8}-[0-9A-F]{4}-4[0-9A-F]{3}-[89AB][0-9A-F]{3}-[0-9A-F]{12}$/i;

export const uuidRegExpLax // free bytes UUID
  = /^[0-9A-F]{8}-[0-9A-F]{4}-[0-9A-F]{4}-[0-9A-F]{4}-[0-9A-F]{12}$/i;

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
  clients: 'clients',
  zones: 'zones',
  details: 'details',
  new: 'new',
  smartContracts: 'smart-contracts',
  logging: 'logging',
  consortiums: 'consortiums',
  organizations: 'organizations',
  system: 'system',
  transactions: 'transactions',
  developer: 'developer',

  get blockchainIdChildren() { return [ // allowed paths under /:blockchainId/*
    this.dashboard, this.blocks, this.nodes, this.smartContracts, this.logging,
    this.consortiums, this.organizations, this.users, this.transactions, this.developer,
    this.zones, this.details
  ]; },

  // Path of /blockchain/*
  blockchain: 'blockchain',
  welcome: 'welcome',
  deploy: 'deploy',
  deploying: 'deploying',

  get blockchainChildren() {
    return [ // allowed paths under /blockchain/*
      this.welcome, this.deploy, this.deploying, this.zones
    ];
  },

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

  base: '/api',

  // Blockchains
  get blockchains() { return `${this.base}/blockchains`; },

  // Consortiums
  get consortiums() { return `${this.base}/consortiums`; },
  get organizations() { return `${this.base}/organizations`; },

  // Contract tools
  get contractTools() { return `${this.base}/concord/contracts`; },
  get contractCompileVersions() { return `${this.contractTools}/compiler_versions`; },
  get contractCompile() { return `${this.contractTools}/compile`; },

  // API Swagger yaml location
  get swaggerYAML() { return `${this.base}/static/api.yaml`; },

  // Task
  get tasks() { return `${this.base}/tasks`; },

  // Features
  get features() { return `${this.base}/features`; },

  // Zone Family
  get zones() { return `${this.blockchains}/zones`; },
  get zonesReload() { return `${this.zones}?action=reload`; },
  get zonesTestConnection() { return `${this.zones}?action=test`; },

  // Metrics
  get metricsWavefront() { return `${this.base}/metrics/wavefront`; },

  // Node Size Template
  get nodeSizeTemplate() {
    return `${this.blockchains}/node-size-template`;
  },

  /**
   * API that requires `blockchainId` already resolved.
   * e.g. `/blockchains/{bid}*`
   */
   // base api path for a specific blockchain
  specificBlockchain(bId: string) { return `${this.blockchains}/${bId}`; },

  // Node (Replica) related
  nodes(bId: string) { return `${this.specificBlockchain(bId)}/replicas`; },
  clients(bId: string) { return `${this.specificBlockchain(bId)}/clients`; },
  committerNodeCredentials(bId: string, nodeId: string) { return `${this.nodes(bId)}/${nodeId}/credentials`; },
  clientNodeCredentials(bId: string, nodeId: string) { return `${this.clients(bId)}/${nodeId}/credentials`; },

  // ! Deprecated
  members(bId: string) { return `${this.specificBlockchain(bId)}/members`; },



  /**
   * API that requires `blockchainId` already resolved with `concord` prefix
   * e.g. `/blockchains/{bid}/concord*`
   */
  specificConcord(bId: string) { return `${this.specificBlockchain(bId)}/concord`; },

  // Blocks related
  blocks(bId: string) { return `${this.specificConcord(bId)}/blocks`; },
  block(bId: string, id) { return `${this.specificConcord(bId)}/blocks/${id}`; },

  // Smart contract related
  contracts(bId: string) { return `${this.specificConcord(bId)}/contracts`; },
  contract(bId: string, id) { return `${this.specificConcord(bId)}/contracts/${id}`; },
  contractVersion(bId: string, id, version) { return `${this.specificConcord(bId)}/contracts/${id}/versions/${version}`; },

  // Node (Replica) related
  ethrpc(bId: string) { return `${this.specificConcord(bId)}/eth`; },

  // Transactions related
  transactions(bId: string) { return `${this.specificConcord(bId)}/transactions`; },
  transaction(bId: string, id) { return `${this.specificConcord(bId)}/transactions/${id}`; },

};
