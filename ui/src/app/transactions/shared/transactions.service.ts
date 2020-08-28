/*
 * Copyright 2018-2019 VMware, all rights reserved.
 */

import { Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { forkJoin as observableForkJoin } from 'rxjs';
import { mergeMap } from 'rxjs/operators';

import { Transaction, TransactionListing } from './transactions.model';
import { BlocksService } from '../../blocks/shared/blocks.service';
import { Apis } from '../../shared/urls.model';
import { BlockchainService } from '../../blockchain/shared/blockchain.service';

@Injectable({
  providedIn: 'root'
})
export class TransactionsService {

  constructor(
    private httpClient: HttpClient,
    private blockchainService: BlockchainService,
    private blocksService: BlocksService,
  ) { }

  getTransaction(transactionHash: string) {
    return this.httpClient.get<Transaction>(
      Apis.transaction(this.blockchainService.blockchainId, transactionHash));
  }

  getTransactions(count: number) {
    const params = new HttpParams().set('count', count.toString());
    return this.httpClient.get<TransactionListing>(
      Apis.transactions(this.blockchainService.blockchainId), {params: params});
  }

  getBlockTransactions(blockNumber: number) {
    // Get blocks, then get individual block, then get individual transactions for all blocks.
    // This is temporary until there is an endpoint to fetch recent transactions
    return this.blocksService.getBlock(blockNumber).pipe(mergeMap(blockResp => {
      return observableForkJoin(blockResp.transactions.map(transaction => this.getTransaction(transaction.hash)));
    }));
  }

  getRecentTransactions(count: number = 1000) {
    // Get blocks, then get individual block, then get individual transactions for all blocks.
    // This is temporary until there is an endpoint to fetch recent transactions
    return this.blocksService.getBlocks(count).pipe(mergeMap(resp => {
      const blockObservables = resp.blocks.map((block) => this.blocksService.getBlock(block.number));
      return observableForkJoin(blockObservables).pipe(mergeMap(blocksResp => {
        const transactionsOverAllBlocks: any[] = blocksResp.map(block => block.transactions).reduce((acc, val) => acc.concat(val), []);
        return observableForkJoin(transactionsOverAllBlocks.map(transaction => this.getTransaction(transaction.hash)));
      }));
    }));
  }
}
