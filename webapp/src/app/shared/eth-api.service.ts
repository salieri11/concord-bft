/*
 * Copyright 2018 VMware, all rights reserved.
 */

import { Inject, Injectable } from '@angular/core';
import { ETHEREUM_API_PREFIX } from './shared.config';
import { HttpClient } from '@angular/common/http';
import {
  EthRequest,
  EthSendTransactionParams,
  EthGetTransactionReceiptParams,
  EthSendTransactionResponse,
  EthGetTransactionReceiptResponse
} from './remote-interfaces';

@Injectable()
export class EthApiService {

  constructor(@Inject(ETHEREUM_API_PREFIX) private ethereumApiPrefix: string, private httpClient: HttpClient) {}

  sendTransaction(params: EthSendTransactionParams) {
    const request: EthRequest = {
      id: 1,
      jsonrpc: '2.0',
      method: 'eth_sendTransaction',
      params: [params]
    };
    return this.httpClient.post<EthSendTransactionResponse>(this.ethereumApiPrefix, request);
  }

  getTransactionReceipt(hash: EthGetTransactionReceiptParams) {
    const request: EthRequest = {
      id: 1,
      jsonrpc: '2.0',
      method: 'eth_getTransactionReceipt',
      params: [hash]
    };
    return this.httpClient.post<EthGetTransactionReceiptResponse>(this.ethereumApiPrefix, request);
  }
}
