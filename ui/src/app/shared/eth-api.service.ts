/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

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
import { Apis } from './urls.model';
import { BlockchainService } from '../blockchain/shared/blockchain.service';


const DEFAULT_BLOCK_PARAMETER = 'latest';

@Injectable({
  providedIn: 'root'
})
export class EthApiService {

  constructor(
    private httpClient: HttpClient,
    private blockchainService: BlockchainService,
  ) { }

  createWallet(params: PersonalNewAccountParams): Observable<PersonalNewAccountResponse> {
    const request: EthRequest = {
      id: 1,
      jsonrpc: '2.0',
      method: 'personal_newAccount',
      params: [params]
    };
    return this.httpClient.post<PersonalNewAccountResponse>(
      Apis.ethrpc(this.blockchainService.blockchainId), request);
  }

  sendTransaction(params: EthSendTransactionParams): Observable<EthSendTransactionResponse> {
    const request: EthRequest = {
      id: 1,
      jsonrpc: '2.0',
      method: 'eth_sendTransaction',
      params: [params]
    };
    return this.httpClient.post<EthSendTransactionResponse>(
      Apis.ethrpc(this.blockchainService.blockchainId), request);
  }

  sendCall(params: EthSendCallParams): Observable<EthSendCallResponse> {
    const request: EthRequest = {
      id: 1,
      jsonrpc: '2.0',
      method: 'eth_call',
      params: [params, DEFAULT_BLOCK_PARAMETER]
    };
    return this.httpClient.post<EthSendCallResponse>(
      Apis.ethrpc(this.blockchainService.blockchainId), request);
  }

  getTransactionReceipt(hash: EthGetTransactionReceiptParams): Observable<EthGetTransactionReceiptResponse> {
    const request: EthRequest = {
      id: 1,
      jsonrpc: '2.0',
      method: 'eth_getTransactionReceipt',
      params: [hash]
    };
    return this.httpClient.post<EthGetTransactionReceiptResponse>(
      Apis.ethrpc(this.blockchainService.blockchainId), request);
  }
}
