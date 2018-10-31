/*
 * Copyright 2018 VMware, all rights reserved.
 */

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
  params: [EthSendTransactionParams | EthGetTransactionReceiptParams | EthSendCallParams] | [EthSendCallParams, string];
}

export interface EthSendCallResponse {
  id: number;
  jsonrpc: string;
  result: string;
  error?: {
    message: string
  };
}

export interface EthSendTransactionResponse {
  id: number;
  jsonrpc: string;
  result: string;
  error?: {
    message: string
  };
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
