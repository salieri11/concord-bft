/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Inject, Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

import { ETHEREUM_API_PREFIX } from './shared.config';

import {
  EthRequest,
  EthSendTransactionParams,
  EthGetTransactionReceiptParams,
  EthSendTransactionResponse,
  EthGetTransactionReceiptResponse,
  EthSendCallParams,
  EthSendCallResponse,
  PersonalNewAccountParams,
  PersonalNewAccountResponse
} from './eth-api.model';

const DEFAULT_BLOCK_PARAMETER = 'latest';

@Injectable({
  providedIn: 'root'
})
export class EthApiService {

  constructor(@Inject(ETHEREUM_API_PREFIX) private ethereumApiPrefix: string, private httpClient: HttpClient) {}

  createWallet(params: PersonalNewAccountParams): Observable<PersonalNewAccountResponse> {
    const request: EthRequest = {
      id: 1,
      jsonrpc: '2.0',
      method: 'personal_newAccount',
      params: [params]
    };
    return this.httpClient.post<PersonalNewAccountResponse>(this.ethereumApiPrefix, request);
  }

  sendTransaction(params: EthSendTransactionParams): Observable<EthSendTransactionResponse> {
    const request: EthRequest = {
      id: 1,
      jsonrpc: '2.0',
      method: 'eth_sendTransaction',
      params: [params]
    };
    return this.httpClient.post<EthSendTransactionResponse>(this.ethereumApiPrefix, request);
  }

  sendCall(params: EthSendCallParams): Observable<EthSendCallResponse> {
    const request: EthRequest = {
      id: 1,
      jsonrpc: '2.0',
      method: 'eth_call',
      params: [params, DEFAULT_BLOCK_PARAMETER]
    };
    return this.httpClient.post<EthSendCallResponse>(this.ethereumApiPrefix, request);
  }

  getTransactionReceipt(hash: EthGetTransactionReceiptParams): Observable<EthGetTransactionReceiptResponse> {
    const request: EthRequest = {
      id: 1,
      jsonrpc: '2.0',
      method: 'eth_getTransactionReceipt',
      params: [hash]
    };
    return this.httpClient.post<EthGetTransactionReceiptResponse>(this.ethereumApiPrefix, request);
  }
}
