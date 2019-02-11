/*
 * Copyright 2018-2019 VMware, all rights reserved.
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

export type PersonalNewAccountParams = string;

export interface EthRequest {
  id: number;
  method: string;
  jsonrpc: string;
  params: [EthSendTransactionParams | EthGetTransactionReceiptParams | EthSendCallParams | PersonalNewAccountParams]
    | [EthSendCallParams, string];
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

export interface PersonalNewAccountResponse {
  id: number;
  jsonrpc: string;
  result: string;
}

export interface EthWallet {
  address: string;
  id: string;
  version: number;
  crypto: EthWalletCrypto;
}

export interface EthWalletCrypto {
  cipher: string;
  cipherText: string;
  kdfparams: {
    p: number;
    r: number;
    salt: string;
    dklen: number;
    n: number;
  };
  cipherparams: {
    iv: string;
  };
  kdf: string;
  mac: string;
}
