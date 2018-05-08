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

export interface EthSendCallParams {
  from?: string;
  to?: string;
  data?: string;
  value?: string;
}

export interface EthSendTransactionParams {
  from: string;
  to?: string;
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

