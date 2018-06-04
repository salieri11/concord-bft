/*
 * Copyright 2018 VMware, all rights reserved.
 */

/**
 * GET response of fetching a list of members
 */
export interface Member {
  host: string;
  status: string;
}

/**
 * GET response of fetching a list of blocks
 */
export interface BlockListing {
  blocks: BlockListingBlock[];
  next: string;
}

/**
 * A simplified block provided when fetching a list of blocks
 */
export interface BlockListingBlock {
  number: number;
  hash: string;
  url: string;
}

/**
 * GET response of fetching an individual block
 */
export interface Block {
  hash: string;
  nonce: string;
  parentHash: string;
  number: number;
  size: number;
  transactions: [{hash: string; url: string; }];
}

/**
 * GET response of fetching an individual transaction
 */
export interface Transaction {
  hash: string;
  from: string;
  to?: string;
  contractAddress?: string;
  value: string;
  input: string;
  nonce: number;
  status: number;
}


export interface SmartContract {
  contract_id: string;
  owner: string;
  url?: string;
  versions?: [{
    address: string;
    metadata: {};
    version: string;
    url: string;
  }];
}

export interface SmartContractVersion {
  contractId: string;
  version: string;
  owner: string;
  metadata: SmartContractMetadata;
  address: string;
  bytecode?: string;
  sourcecode?: string;
}

export interface SmartContractMetadata {
  compiler: {
    version: string;
  };
  language: string;
  output: {
    abi: AbiDefinition[];
    devdoc: any;
    userdoc: any;
  };
  settings: any;
  sources: any;
  version: number;
}

export interface AbiFunctionDefinition {
  type: string;
  name: string;
  constant: boolean;
  payable: boolean;
  stateMutability: string;
  inputs: AbiFunctionParameter[];
  outputs: AbiFunctionParameter[];
}

export interface AbiEventDefinition {
  type: string;
  name: string;
  anonymous: boolean;
  inputs: AbiFunctionParameter[];
}

type AbiDefinition = AbiEventDefinition | AbiFunctionDefinition;

export interface AbiFunctionParameter {
  indexed?: boolean;
  type: string;
  name: string;
}

export interface SmartContractCreateRequest {
  id: number;
  contractId: string;
  version: string;
  from: string;
  sourceCode: string;
}

export interface SmartContractCreateResult {
  contractId: string;
  version: string;
  url: string;
}

export interface ApiResponse<T> {
  id: number;
  result: T;
}

export type SmartContractResponse = ApiResponse<SmartContractCreateResult[]>;

export interface EthSendCallParams {
  from?: string;
  to?: string;
  gas?: string;
  data?: string;
  value?: string;
}

export interface EthSendTransactionParams {
  from: string;
  to?: string;
  gas?: string;
  data?: string;
  value?: string;
}

export type EthGetTransactionReceiptParams = string;

export interface EthRequest {
  id: number;
  method: string;
  jsonrpc: string;
  params: [EthSendTransactionParams | EthGetTransactionReceiptParams | EthSendCallParams];
}

export interface EthSendCallResponse {
  id: number;
  jsonrpc: string;
  result: string;
}

export interface EthSendTransactionResponse {
  id: number;
  jsonrpc: string;
  result: string;
}

export interface EthGetTransactionReceiptResponse {
  id: number;
  jsonrpc: string;
  result: {
    status: string;
    transactionHash: string;
    contractAddress: string;
    blockHash: string;
  };
}

