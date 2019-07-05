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

import { BlockchainService } from '../shared/blockchain.service';
import { ConcordApiService } from './concord-api';


const DEFAULT_BLOCK_PARAMETER = 'latest';

@Injectable({
  providedIn: 'root'
})
export class EthApiService extends ConcordApiService {

  constructor(
    // @ts-ignore: no unused locals
    @Inject(ETHEREUM_API_PREFIX) private ethereumApiPrefix: string,
    private httpClient: HttpClient,
    // @ts-ignore: no unused locals
    private blockchainService: BlockchainService
  ) {
    super(ethereumApiPrefix);
  }

  get apiSubPath() {
    return '';
  }

  createWallet(params: PersonalNewAccountParams): Observable<PersonalNewAccountResponse> {
    const request: EthRequest = {
      id: 1,
      jsonrpc: '2.0',
      method: 'personal_newAccount',
      params: [params]
    };
    return this.httpClient.post<PersonalNewAccountResponse>(this.resourcePath(), request);
  }

  sendTransaction(params: EthSendTransactionParams): Observable<EthSendTransactionResponse> {
    const request: EthRequest = {
      id: 1,
      jsonrpc: '2.0',
      method: 'eth_sendTransaction',
      params: [params]
    };
    return this.httpClient.post<EthSendTransactionResponse>(this.resourcePath(), request);
  }

  sendCall(params: EthSendCallParams): Observable<EthSendCallResponse> {
    const request: EthRequest = {
      id: 1,
      jsonrpc: '2.0',
      method: 'eth_call',
      params: [params, DEFAULT_BLOCK_PARAMETER]
    };
    return this.httpClient.post<EthSendCallResponse>(this.resourcePath(), request);
  }

  getTransactionReceipt(hash: EthGetTransactionReceiptParams): Observable<EthGetTransactionReceiptResponse> {
    const request: EthRequest = {
      id: 1,
      jsonrpc: '2.0',
      method: 'eth_getTransactionReceipt',
      params: [hash]
    };
    return this.httpClient.post<EthGetTransactionReceiptResponse>(this.resourcePath(), request);
  }
}
